1: #version 300 es
2: #define attribute in
3: #define varying out
4: #define texture2D texture
5: precision highp float;
6: precision highp int;
7: #define HIGH_PRECISION
8: #define SHADER_NAME ShaderMaterial
9: #define TOON true
10: #define MATCAP true
11: #define MATCAP_BLENDING_ADD true
12: #define VERTEX_TEXTURES
13: #define GAMMA_FACTOR 2
14: #define MAX_BONES 1024
15: #define USE_MAP
16: #define USE_NORMALMAP
17: #define TANGENTSPACE_NORMALMAP
18: #define USE_UV
19: #define USE_SKINNING
20: #define BONE_TEXTURE
21: #define DOUBLE_SIDED
22: #define USE_SHADOWMAP
23: #define SHADOWMAP_TYPE_PCF_SOFT
24: uniform mat4 modelMatrix;
25: uniform mat4 modelViewMatrix;
26: uniform mat4 projectionMatrix;
27: uniform mat4 viewMatrix;
28: uniform mat3 normalMatrix;
29: uniform vec3 cameraPosition;
30: uniform bool isOrthographic;
31: #ifdef USE_INSTANCING
32: 	attribute mat4 instanceMatrix;
33: #endif
34: #ifdef USE_INSTANCING_COLOR
35: 	attribute vec3 instanceColor;
36: #endif
37: attribute vec3 position;
38: attribute vec3 normal;
39: attribute vec2 uv;
40: #ifdef USE_TANGENT
41: 	attribute vec4 tangent;
42: #endif
43: #if defined( USE_COLOR_ALPHA )
44: 	attribute vec4 color;
45: #elif defined( USE_COLOR )
46: 	attribute vec3 color;
47: #endif
48: #ifdef USE_MORPHTARGETS
49: 	attribute vec3 morphTarget0;
50: 	attribute vec3 morphTarget1;
51: 	attribute vec3 morphTarget2;
52: 	attribute vec3 morphTarget3;
53: 	#ifdef USE_MORPHNORMALS
54: 		attribute vec3 morphNormal0;
55: 		attribute vec3 morphNormal1;
56: 		attribute vec3 morphNormal2;
57: 		attribute vec3 morphNormal3;
58: 	#else
59: 		attribute vec3 morphTarget4;
60: 		attribute vec3 morphTarget5;
61: 		attribute vec3 morphTarget6;
62: 		attribute vec3 morphTarget7;
63: 	#endif
64: #endif
65: #ifdef USE_SKINNING
66: 	attribute vec4 skinIndex;
67: 	attribute vec4 skinWeight;
68: #endif
69: 
70: #define PHONG
71: varying vec3 vViewPosition;
72: #ifndef FLAT_SHADED
73: 	varying vec3 vNormal;
74: #endif
75: #define PI 3.141592653589793
76: #define PI2 6.283185307179586
77: #define PI_HALF 1.5707963267948966
78: #define RECIPROCAL_PI 0.3183098861837907
79: #define RECIPROCAL_PI2 0.15915494309189535
80: #define EPSILON 1e-6
81: #ifndef saturate
82: #define saturate(a) clamp( a, 0.0, 1.0 )
83: #endif
84: #define whiteComplement(a) ( 1.0 - saturate( a ) )
85: float pow2( const in float x ) { return x*x; }
86: float pow3( const in float x ) { return x*x*x; }
87: float pow4( const in float x ) { float x2 = x*x; return x2*x2; }
88: float average( const in vec3 color ) { return dot( color, vec3( 0.3333 ) ); }
89: highp float rand( const in vec2 uv ) {
90: 	const highp float a = 12.9898, b = 78.233, c = 43758.5453;
91: 	highp float dt = dot( uv.xy, vec2( a,b ) ), sn = mod( dt, PI );
92: 	return fract(sin(sn) * c);
93: }
94: #ifdef HIGH_PRECISION
95: 	float precisionSafeLength( vec3 v ) { return length( v ); }
96: #else
97: 	float max3( vec3 v ) { return max( max( v.x, v.y ), v.z ); }
98: 	float precisionSafeLength( vec3 v ) {
99: 		float maxComponent = max3( abs( v ) );
100: 		return length( v / maxComponent ) * maxComponent;
101: 	}
102: #endif
103: struct IncidentLight {
104: 	vec3 color;
105: 	vec3 direction;
106: 	bool visible;
107: };
108: struct ReflectedLight {
109: 	vec3 directDiffuse;
110: 	vec3 directSpecular;
111: 	vec3 indirectDiffuse;
112: 	vec3 indirectSpecular;
113: };
114: struct GeometricContext {
115: 	vec3 position;
116: 	vec3 normal;
117: 	vec3 viewDir;
118: #ifdef CLEARCOAT
119: 	vec3 clearcoatNormal;
120: #endif
121: };
122: vec3 transformDirection( in vec3 dir, in mat4 matrix ) {
123: 	return normalize( ( matrix * vec4( dir, 0.0 ) ).xyz );
124: }
125: vec3 inverseTransformDirection( in vec3 dir, in mat4 matrix ) {
126: 	return normalize( ( vec4( dir, 0.0 ) * matrix ).xyz );
127: }
128: vec3 projectOnPlane(in vec3 point, in vec3 pointOnPlane, in vec3 planeNormal ) {
129: 	float distance = dot( planeNormal, point - pointOnPlane );
130: 	return - distance * planeNormal + point;
131: }
132: float sideOfPlane( in vec3 point, in vec3 pointOnPlane, in vec3 planeNormal ) {
133: 	return sign( dot( point - pointOnPlane, planeNormal ) );
134: }
135: vec3 linePlaneIntersect( in vec3 pointOnLine, in vec3 lineDirection, in vec3 pointOnPlane, in vec3 planeNormal ) {
136: 	return lineDirection * ( dot( planeNormal, pointOnPlane - pointOnLine ) / dot( planeNormal, lineDirection ) ) + pointOnLine;
137: }
138: mat3 transposeMat3( const in mat3 m ) {
139: 	mat3 tmp;
140: 	tmp[ 0 ] = vec3( m[ 0 ].x, m[ 1 ].x, m[ 2 ].x );
141: 	tmp[ 1 ] = vec3( m[ 0 ].y, m[ 1 ].y, m[ 2 ].y );
142: 	tmp[ 2 ] = vec3( m[ 0 ].z, m[ 1 ].z, m[ 2 ].z );
143: 	return tmp;
144: }
145: float linearToRelativeLuminance( const in vec3 color ) {
146: 	vec3 weights = vec3( 0.2126, 0.7152, 0.0722 );
147: 	return dot( weights, color.rgb );
148: }
149: bool isPerspectiveMatrix( mat4 m ) {
150: 	return m[ 2 ][ 3 ] == - 1.0;
151: }
152: vec2 equirectUv( in vec3 dir ) {
153: 	float u = atan( dir.z, dir.x ) * RECIPROCAL_PI2 + 0.5;
154: 	float v = asin( clamp( dir.y, - 1.0, 1.0 ) ) * RECIPROCAL_PI + 0.5;
155: 	return vec2( u, v );
156: }
157: #ifdef USE_UV
158: 	#ifdef UVS_VERTEX_ONLY
159: 		vec2 vUv;
160: 	#else
161: 		varying vec2 vUv;
162: 	#endif
163: 	uniform mat3 uvTransform;
164: #endif
165: #if defined( USE_LIGHTMAP ) || defined( USE_AOMAP )
166: 	attribute vec2 uv2;
167: 	varying vec2 vUv2;
168: 	uniform mat3 uv2Transform;
169: #endif
170: #ifdef USE_DISPLACEMENTMAP
171: 	uniform sampler2D displacementMap;
172: 	uniform float displacementScale;
173: 	uniform float displacementBias;
174: #endif
175: #ifdef USE_ENVMAP
176: 	#if defined( USE_BUMPMAP ) || defined( USE_NORMALMAP ) ||defined( PHONG )
177: 		#define ENV_WORLDPOS
178: 	#endif
179: 	#ifdef ENV_WORLDPOS
180: 		
181: 		varying vec3 vWorldPosition;
182: 	#else
183: 		varying vec3 vReflect;
184: 		uniform float refractionRatio;
185: 	#endif
186: #endif
187: #if defined( USE_COLOR_ALPHA )
188: 	varying vec4 vColor;
189: #elif defined( USE_COLOR ) || defined( USE_INSTANCING_COLOR )
190: 	varying vec3 vColor;
191: #endif
192: #ifdef USE_FOG
193: 	varying float fogDepth;
194: #endif
195: #ifdef USE_MORPHTARGETS
196: 	uniform float morphTargetBaseInfluence;
197: 	#ifndef USE_MORPHNORMALS
198: 		uniform float morphTargetInfluences[ 8 ];
199: 	#else
200: 		uniform float morphTargetInfluences[ 4 ];
201: 	#endif
202: #endif
203: #ifdef USE_SKINNING
204: 	uniform mat4 bindMatrix;
205: 	uniform mat4 bindMatrixInverse;
206: 	#ifdef BONE_TEXTURE
207: 		uniform highp sampler2D boneTexture;
208: 		uniform int boneTextureSize;
209: 		mat4 getBoneMatrix( const in float i ) {
210: 			float j = i * 4.0;
211: 			float x = mod( j, float( boneTextureSize ) );
212: 			float y = floor( j / float( boneTextureSize ) );
213: 			float dx = 1.0 / float( boneTextureSize );
214: 			float dy = 1.0 / float( boneTextureSize );
215: 			y = dy * ( y + 0.5 );
216: 			vec4 v1 = texture2D( boneTexture, vec2( dx * ( x + 0.5 ), y ) );
217: 			vec4 v2 = texture2D( boneTexture, vec2( dx * ( x + 1.5 ), y ) );
218: 			vec4 v3 = texture2D( boneTexture, vec2( dx * ( x + 2.5 ), y ) );
219: 			vec4 v4 = texture2D( boneTexture, vec2( dx * ( x + 3.5 ), y ) );
220: 			mat4 bone = mat4( v1, v2, v3, v4 );
221: 			return bone;
222: 		}
223: 	#else
224: 		uniform mat4 boneMatrices[ MAX_BONES ];
225: 		mat4 getBoneMatrix( const in float i ) {
226: 			mat4 bone = boneMatrices[ int(i) ];
227: 			return bone;
228: 		}
229: 	#endif
230: #endif
231: #ifdef USE_SHADOWMAP
232: 	#if 1 > 0
233: 		uniform mat4 directionalShadowMatrix[ 1 ];
234: 		varying vec4 vDirectionalShadowCoord[ 1 ];
235: 		struct DirectionalLightShadow {
236: 			float shadowBias;
237: 			float shadowNormalBias;
238: 			float shadowRadius;
239: 			vec2 shadowMapSize;
240: 		};
241: 		uniform DirectionalLightShadow directionalLightShadows[ 1 ];
242: 	#endif
243: 	#if 0 > 0
244: 		uniform mat4 spotShadowMatrix[ 0 ];
245: 		varying vec4 vSpotShadowCoord[ 0 ];
246: 		struct SpotLightShadow {
247: 			float shadowBias;
248: 			float shadowNormalBias;
249: 			float shadowRadius;
250: 			vec2 shadowMapSize;
251: 		};
252: 		uniform SpotLightShadow spotLightShadows[ 0 ];
253: 	#endif
254: 	#if 0 > 0
255: 		uniform mat4 pointShadowMatrix[ 0 ];
256: 		varying vec4 vPointShadowCoord[ 0 ];
257: 		struct PointLightShadow {
258: 			float shadowBias;
259: 			float shadowNormalBias;
260: 			float shadowRadius;
261: 			vec2 shadowMapSize;
262: 			float shadowCameraNear;
263: 			float shadowCameraFar;
264: 		};
265: 		uniform PointLightShadow pointLightShadows[ 0 ];
266: 	#endif
267: #endif
268: #ifdef USE_LOGDEPTHBUF
269: 	#ifdef USE_LOGDEPTHBUF_EXT
270: 		varying float vFragDepth;
271: 		varying float vIsPerspective;
272: 	#else
273: 		uniform float logDepthBufFC;
274: 	#endif
275: #endif
276: #if 0 > 0
277: 	varying vec3 vClipPosition;
278: #endif
279: void main() {
280: #ifdef USE_UV
281: 	vUv = ( uvTransform * vec3( uv, 1 ) ).xy;
282: #endif
283: #if defined( USE_LIGHTMAP ) || defined( USE_AOMAP )
284: 	vUv2 = ( uv2Transform * vec3( uv2, 1 ) ).xy;
285: #endif
286: #if defined( USE_COLOR_ALPHA )
287: 	vColor = vec4( 1.0 );
288: #elif defined( USE_COLOR ) || defined( USE_INSTANCING_COLOR )
289: 	vColor = vec3( 1.0 );
290: #endif
291: #ifdef USE_COLOR
292: 	vColor *= color;
293: #endif
294: #ifdef USE_INSTANCING_COLOR
295: 	vColor.xyz *= instanceColor.xyz;
296: #endif
297: vec3 objectNormal = vec3( normal );
298: #ifdef USE_TANGENT
299: 	vec3 objectTangent = vec3( tangent.xyz );
300: #endif
301: #ifdef USE_MORPHNORMALS
302: 	objectNormal *= morphTargetBaseInfluence;
303: 	objectNormal += morphNormal0 * morphTargetInfluences[ 0 ];
304: 	objectNormal += morphNormal1 * morphTargetInfluences[ 1 ];
305: 	objectNormal += morphNormal2 * morphTargetInfluences[ 2 ];
306: 	objectNormal += morphNormal3 * morphTargetInfluences[ 3 ];
307: #endif
308: #ifdef USE_SKINNING
309: 	mat4 boneMatX = getBoneMatrix( skinIndex.x );
310: 	mat4 boneMatY = getBoneMatrix( skinIndex.y );
311: 	mat4 boneMatZ = getBoneMatrix( skinIndex.z );
312: 	mat4 boneMatW = getBoneMatrix( skinIndex.w );
313: #endif
314: #ifdef USE_SKINNING
315: 	mat4 skinMatrix = mat4( 0.0 );
316: 	skinMatrix += skinWeight.x * boneMatX;
317: 	skinMatrix += skinWeight.y * boneMatY;
318: 	skinMatrix += skinWeight.z * boneMatZ;
319: 	skinMatrix += skinWeight.w * boneMatW;
320: 	skinMatrix = bindMatrixInverse * skinMatrix * bindMatrix;
321: 	objectNormal = vec4( skinMatrix * vec4( objectNormal, 0.0 ) ).xyz;
322: 	#ifdef USE_TANGENT
323: 		objectTangent = vec4( skinMatrix * vec4( objectTangent, 0.0 ) ).xyz;
324: 	#endif
325: #endif
326: vec3 transformedNormal = objectNormal;
327: #ifdef USE_INSTANCING
328: 	mat3 m = mat3( instanceMatrix );
329: 	transformedNormal /= vec3( dot( m[ 0 ], m[ 0 ] ), dot( m[ 1 ], m[ 1 ] ), dot( m[ 2 ], m[ 2 ] ) );
330: 	transformedNormal = m * transformedNormal;
331: #endif
332: transformedNormal = normalMatrix * transformedNormal;
333: #ifdef FLIP_SIDED
334: 	transformedNormal = - transformedNormal;
335: #endif
336: #ifdef USE_TANGENT
337: 	vec3 transformedTangent = ( modelViewMatrix * vec4( objectTangent, 0.0 ) ).xyz;
338: 	#ifdef FLIP_SIDED
339: 		transformedTangent = - transformedTangent;
340: 	#endif
341: #endif
342: #ifndef FLAT_SHADED
343: 	vNormal = normalize( transformedNormal );
344: #endif
345: vec3 transformed = vec3( position );
346: #ifdef USE_MORPHTARGETS
347: 	transformed *= morphTargetBaseInfluence;
348: 	transformed += morphTarget0 * morphTargetInfluences[ 0 ];
349: 	transformed += morphTarget1 * morphTargetInfluences[ 1 ];
350: 	transformed += morphTarget2 * morphTargetInfluences[ 2 ];
351: 	transformed += morphTarget3 * morphTargetInfluences[ 3 ];
352: 	#ifndef USE_MORPHNORMALS
353: 		transformed += morphTarget4 * morphTargetInfluences[ 4 ];
354: 		transformed += morphTarget5 * morphTargetInfluences[ 5 ];
355: 		transformed += morphTarget6 * morphTargetInfluences[ 6 ];
356: 		transformed += morphTarget7 * morphTargetInfluences[ 7 ];
357: 	#endif
358: #endif
359: #ifdef USE_SKINNING
360: 	vec4 skinVertex = bindMatrix * vec4( transformed, 1.0 );
361: 	vec4 skinned = vec4( 0.0 );
362: 	skinned += boneMatX * skinVertex * skinWeight.x;
363: 	skinned += boneMatY * skinVertex * skinWeight.y;
364: 	skinned += boneMatZ * skinVertex * skinWeight.z;
365: 	skinned += boneMatW * skinVertex * skinWeight.w;
366: 	transformed = ( bindMatrixInverse * skinned ).xyz;
367: #endif
368: #ifdef USE_DISPLACEMENTMAP
369: 	transformed += normalize( objectNormal ) * ( texture2D( displacementMap, vUv ).x * displacementScale + displacementBias );
370: #endif
371: vec4 mvPosition = vec4( transformed, 1.0 );
372: #ifdef USE_INSTANCING
373: 	mvPosition = instanceMatrix * mvPosition;
374: #endif
375: mvPosition = modelViewMatrix * mvPosition;
376: gl_Position = projectionMatrix * mvPosition;
377: #ifdef USE_LOGDEPTHBUF
378: 	#ifdef USE_LOGDEPTHBUF_EXT
379: 		vFragDepth = 1.0 + gl_Position.w;
380: 		vIsPerspective = float( isPerspectiveMatrix( projectionMatrix ) );
381: 	#else
382: 		if ( isPerspectiveMatrix( projectionMatrix ) ) {
383: 			gl_Position.z = log2( max( EPSILON, gl_Position.w + 1.0 ) ) * logDepthBufFC - 1.0;
384: 			gl_Position.z *= gl_Position.w;
385: 		}
386: 	#endif
387: #endif
388: #if 0 > 0
389: 	vClipPosition = - mvPosition.xyz;
390: #endif
391: 	vViewPosition = - mvPosition.xyz;
392: #if defined( USE_ENVMAP ) || defined( DISTANCE ) || defined ( USE_SHADOWMAP ) || defined ( USE_TRANSMISSION )
393: 	vec4 worldPosition = vec4( transformed, 1.0 );
394: 	#ifdef USE_INSTANCING
395: 		worldPosition = instanceMatrix * worldPosition;
396: 	#endif
397: 	worldPosition = modelMatrix * worldPosition;
398: #endif
399: #ifdef USE_ENVMAP
400: 	#ifdef ENV_WORLDPOS
401: 		vWorldPosition = worldPosition.xyz;
402: 	#else
403: 		vec3 cameraToVertex;
404: 		if ( isOrthographic ) {
405: 			cameraToVertex = normalize( vec3( - viewMatrix[ 0 ][ 2 ], - viewMatrix[ 1 ][ 2 ], - viewMatrix[ 2 ][ 2 ] ) );
406: 		} else {
407: 			cameraToVertex = normalize( worldPosition.xyz - cameraPosition );
408: 		}
409: 		vec3 worldNormal = inverseTransformDirection( transformedNormal, viewMatrix );
410: 		#ifdef ENVMAP_MODE_REFLECTION
411: 			vReflect = reflect( cameraToVertex, worldNormal );
412: 		#else
413: 			vReflect = refract( cameraToVertex, worldNormal, refractionRatio );
414: 		#endif
415: 	#endif
416: #endif
417: #ifdef USE_SHADOWMAP
418: 	#if 1 > 0 || 0 > 0 || 0 > 0
419: 		vec3 shadowWorldNormal = inverseTransformDirection( transformedNormal, viewMatrix );
420: 		vec4 shadowWorldPosition;
421: 	#endif
422: 	#if 1 > 0
423: 	
424: 		shadowWorldPosition = worldPosition + vec4( shadowWorldNormal * directionalLightShadows[ 0 ].shadowNormalBias, 0 );
425: 		vDirectionalShadowCoord[ 0 ] = directionalShadowMatrix[ 0 ] * shadowWorldPosition;
426: 	
427: 	#endif
428: 	#if 0 > 0
429: 	
430: 	#endif
431: 	#if 0 > 0
432: 	
433: 	#endif
434: #endif
435: #ifdef USE_FOG
436: 	fogDepth = - mvPosition.z;
437: #endif
438: }
