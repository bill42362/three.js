( function () {

	/**
 * MMD Toon Shader
 *
 * This shader is extended from MashPhongMaterial, and merged algorithms with
 * MashToonMaterial and MeshMetcapMaterial.
 * Ideas came from https://github.com/mrdoob/three.js/issues/19609
 *
 * Combining steps:
 *  * Declare matcap uniform.
 *  * Add gradientmap_pars_fragment.
 *  * Combine dotNL and gradient irradiances as total irradiance.
 *    (Replace lights_phong_pars_fragment with lights_mmd_toon_pars_fragment)
 *  * Add mmd_toon_matcap_fragment.
 */
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
	irradiance += saturate( dot( geometry.normal, directLight.direction ) ) * directLight.color;
  irradiance *= 0.5;

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
		uniforms: THREE.UniformsUtils.merge( [ THREE.ShaderLib.toon.uniforms, THREE.ShaderLib.phong.uniforms, THREE.ShaderLib.matcap.uniforms ] ),
		vertexShader: `
    #define TOON
    #define MATCAP
  ` + THREE.ShaderLib.phong.vertexShader,
		fragmentShader: THREE.ShaderLib.phong.fragmentShader.replace( '#include <common>', `
    		  #ifdef USE_MATCAP
    		    uniform sampler2D matcap;
    		  #endif

    		  #include <common>
    		` ).replace( '#include <envmap_common_pars_fragment>', `
    		  #include <gradientmap_pars_fragment>
    		  #include <envmap_common_pars_fragment>
    		` ).replace( '#include <lights_phong_pars_fragment>', lights_mmd_toon_pars_fragment ).replace( '#include <envmap_fragment>', `
    		  #include <envmap_fragment>
    		  ${mmd_toon_matcap_fragment}
    		` )
	};

	THREE.MMDToonShader = MMDToonShader;

} )();
