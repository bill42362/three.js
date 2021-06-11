/**
 * MMD Toon Shader
 *
 * This shader is extended from MeshPhongMaterial, and merged algorithms with
 * MeshToonMaterial and MeshMetcapMaterial.
 * Ideas came from https://github.com/mrdoob/three.js/issues/19609
 *
 * Combining steps:
 *  * Declare matcap uniform.
 *  * Add gradientmap_pars_fragment.
 *  * Use gradient irradiances instead of dotNL irradiance from MeshPhongMaterial.
 *    (Replace lights_phong_pars_fragment with lights_mmd_toon_pars_fragment)
 *  * Add mmd_toon_matcap_fragment.
 */

import { Vector3, UniformsUtils, ShaderLib } from '../../../build/three.module.js';

const mmd_toon_morphtarget_pars_vertex = `
attribute float vertexIndex;
uniform vec3 morphMetadata; // x: targets count, y: vectors texture width, z: element texture width
uniform sampler2D morphTargetInfluences;
uniform sampler2D morphDataVectors;
uniform sampler2D morphDataElementIndexs;
uniform sampler2D morphDataElementValues;
`;

const mmd_toon_morphtarget_vertex = `
int morphTargetsCount = int(morphMetadata.x);
for(int i = 0; i < morphTargetsCount; ++i) {
  float iFloat = float(i);
  float influence = texture2D(morphTargetInfluences, vec2(iFloat / 128.0, 0.0)).r;
  if (influence == 0.0) {
    continue;
  }

  float vectorX = mod(iFloat, morphMetadata.y);
  float vectorY = ((iFloat - vectorX) / morphMetadata.y);

  vec4 morphDataVector = texture2D(morphDataVectors, vec2(vectorX / morphMetadata.y, vectorY / morphMetadata.y));
  // .r: first segment index, .g: segments count
  if (morphDataVector.g == 0.0) {
    continue;
  }

  bool neverInTarget = false;
  bool isInTarget = false;
  float segmentsIndex = morphDataVector.r;
  int segmentsCount = int(morphDataVector.g);
  float firstElementIndex = 0.0;

  for(int iSegment = 0; iSegment < segmentsCount && !isInTarget && !neverInTarget; ++iSegment) {
    float segmentIndex = segmentsIndex + float(iSegment);
    vectorX = mod(segmentIndex, morphMetadata.y);
    vectorY = ((segmentIndex - vectorX) / morphMetadata.y);
    morphDataVector = texture2D(morphDataVectors, vec2(vectorX / morphMetadata.y, vectorY / morphMetadata.y));

    float dataX = mod(morphDataVector.b, morphMetadata.z);
    float dataY = ((morphDataVector.b - dataX) / morphMetadata.z);

    firstElementIndex = texture2D(morphDataElementIndexs, vec2(dataX / morphMetadata.z, dataY / morphMetadata.z)).r;
    if (firstElementIndex > vertexIndex) {
      neverInTarget = true;
    }
    if (firstElementIndex <= vertexIndex && (firstElementIndex + morphDataVector.a) > vertexIndex) {
      isInTarget = true;
    }
  }

  if (!isInTarget) {
    continue;
  }

  float valueIndex = morphDataVector.b + vertexIndex - firstElementIndex;
  float valueX = mod(valueIndex, morphMetadata.z);
  float valueY = ((valueIndex - valueX) / morphMetadata.z);
  vec4 value = texture2D(morphDataElementValues, vec2(valueX / morphMetadata.z, valueY / morphMetadata.z));

  transformed += value.xyz * influence;
}
`;

const phong_fragment = `
#define PHONG
uniform vec3 diffuse;
uniform vec3 emissive;
uniform vec3 specular;
uniform float shininess;
uniform float opacity;
#include <common>
#include <packing>
#include <dithering_pars_fragment>
#include <color_pars_fragment>
#include <uv_pars_fragment>
#include <uv2_pars_fragment>
#include <map_pars_fragment>
#include <alphamap_pars_fragment>
#include <aomap_pars_fragment>
#include <lightmap_pars_fragment>
#include <emissivemap_pars_fragment>
#include <envmap_common_pars_fragment>
#include <envmap_pars_fragment>
#include <cube_uv_reflection_fragment>
#include <fog_pars_fragment>
#include <bsdfs>
#include <lights_pars_begin>
#include <lights_phong_pars_fragment>
#include <shadowmap_pars_fragment>
#include <bumpmap_pars_fragment>
#include <normalmap_pars_fragment>
#include <specularmap_pars_fragment>
#include <logdepthbuf_pars_fragment>
#include <clipping_planes_pars_fragment>

void main() {
	#include <clipping_planes_fragment>
	vec4 diffuseColor = vec4( diffuse, opacity );
	ReflectedLight reflectedLight = ReflectedLight( vec3( 0.0 ), vec3( 0.0 ), vec3( 0.0 ), vec3( 0.0 ) );
	vec3 totalEmissiveRadiance = emissive;
	#include <logdepthbuf_fragment>
	#include <map_fragment>
	#include <color_fragment>
	#include <alphamap_fragment>
	#include <alphatest_fragment>
	#include <specularmap_fragment>
	#include <normal_fragment_begin>
	#include <normal_fragment_maps>
	#include <emissivemap_fragment>
	// accumulation
	#include <lights_phong_fragment>
	#include <lights_fragment_begin>
	#include <lights_fragment_maps>
	#include <lights_fragment_end>
	// modulation
	#include <aomap_fragment>
	vec3 outgoingLight = reflectedLight.directDiffuse + reflectedLight.indirectDiffuse + reflectedLight.directSpecular + reflectedLight.indirectSpecular + totalEmissiveRadiance;
	#include <envmap_fragment>
	gl_FragColor = vec4( outgoingLight, diffuseColor.a );
	#include <tonemapping_fragment>
	#include <encodings_fragment>
	#include <fog_fragment>
	#include <premultiplied_alpha_fragment>
	#include <dithering_fragment>
}
`;

const lights_mmd_toon_pars_fragment = `
varying vec3 vViewPosition;

#ifndef FLAT_SHADED

	varying vec3 vNormal;

#endif


struct BlinnPhongMaterial {

	vec3 diffuseColor;
	vec3 specularColor;
	float specularShininess;
	float specularStrength;

};

void RE_Direct_BlinnPhong( const in IncidentLight directLight, const in GeometricContext geometry, const in BlinnPhongMaterial material, inout ReflectedLight reflectedLight ) {

	vec3 irradiance = getGradientIrradiance( geometry.normal, directLight.direction ) * directLight.color;

	#ifndef PHYSICALLY_CORRECT_LIGHTS

		irradiance *= PI; // punctual light

	#endif

	reflectedLight.directDiffuse += irradiance * BRDF_Diffuse_Lambert( material.diffuseColor );

	reflectedLight.directSpecular += irradiance * BRDF_Specular_BlinnPhong( directLight, geometry, material.specularColor, material.specularShininess ) * material.specularStrength;

}

void RE_IndirectDiffuse_BlinnPhong( const in vec3 irradiance, const in GeometricContext geometry, const in BlinnPhongMaterial material, inout ReflectedLight reflectedLight ) {

	reflectedLight.indirectDiffuse += irradiance * BRDF_Diffuse_Lambert( material.diffuseColor );

}

#define RE_Direct				RE_Direct_BlinnPhong
#define RE_IndirectDiffuse		RE_IndirectDiffuse_BlinnPhong

#define Material_LightProbeLOD( material )	(0)
`;

const mmd_toon_matcap_fragment = `
#ifdef USE_MATCAP

	vec3 viewDir = normalize( vViewPosition );
	vec3 x = normalize( vec3( viewDir.z, 0.0, - viewDir.x ) );
	vec3 y = cross( viewDir, x );
	vec2 uv = vec2( dot( x, normal ), dot( y, normal ) ) * 0.495 + 0.5; // 0.495 to remove artifacts caused by undersized matcap disks
	vec4 matcapColor = texture2D( matcap, uv );
	matcapColor = matcapTexelToLinear( matcapColor );

	#ifdef MATCAP_BLENDING_MULTIPLY

		outgoingLight *= matcapColor.rgb;

	#elif defined( MATCAP_BLENDING_ADD )

		outgoingLight += matcapColor.rgb;

	#endif

#endif
`;

const MMDToonShader = {

	defines: {
		TOON: true,
		MATCAP: true,
		MATCAP_BLENDING_ADD: true,
	},

	uniforms: UniformsUtils.merge( [
		ShaderLib.toon.uniforms,
		ShaderLib.phong.uniforms,
		ShaderLib.matcap.uniforms,
    {
      morphMetadata: { value: new Vector3(0, 0, 0) },
      morphTargetInfluences: { value: [] },
      morphDataVectors: { value: null },
      morphDataElementIndexs: { value: null },
      morphDataElementValues: { value: null },
    },
	] ),

	vertexShader:
    ShaderLib.phong.vertexShader
      .replace(
        '#include <morphtarget_pars_vertex>',
        mmd_toon_morphtarget_pars_vertex
      )
      .replace(
        '#include <morphtarget_vertex>',
        mmd_toon_morphtarget_vertex
      ),

	fragmentShader:
		ShaderLib.phong.fragmentShader
			.replace(
				'#include <common>',
				`
					#ifdef USE_MATCAP
						uniform sampler2D matcap;
					#endif

					#include <common>
				`
			)
			.replace(
				'#include <envmap_common_pars_fragment>',
				`
					#include <gradientmap_pars_fragment>
					#include <envmap_common_pars_fragment>
				`
			)
			.replace(
				'#include <lights_phong_pars_fragment>',
				lights_mmd_toon_pars_fragment
			)
			.replace(
				'#include <envmap_fragment>',
				`
					#include <envmap_fragment>
					${mmd_toon_matcap_fragment}
				`
			),

};

export { MMDToonShader };
