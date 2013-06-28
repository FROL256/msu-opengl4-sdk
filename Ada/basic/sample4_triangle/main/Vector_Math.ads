with Interfaces;
with Ada.Numerics.Float_Random;
with Generic_Vector_Math;

use Interfaces;

package Vector_Math is

  -- float math
  --
  package Float_Math is new Generic_Vector_Math (float);

  function safe_tan(x : float) return float;
  infinity : constant float := float'Last;

  type float2 is new Float_Math.vector2;
  type float3 is new Float_Math.vector3;
  type float4 is new Float_Math.vector4;
  type float4x4 is new Float_Math.Matrix4;

  function sqr(x : float) return float renames Float_Math.sqr;
  function min(a, b : float) return float renames Float_Math.min;
  function max(a, b : float) return float renames Float_Math.max;
  function min(a, b, c : float) return float renames Float_Math.min;
  function max(a, b, c : float) return float renames Float_Math.max;
  function clamp(x,a,b : float) return float renames Float_Math.clamp;

  function length(a : float3) return float;
  function normalize (a : float3) return float3;
  function reflect(dir : float3; normal: float3) return float3;

  function min(a, b : Float_Math.vector3) return Float_Math.vector3 renames Float_Math.min;
  function max(a, b : Float_Math.vector3) return Float_Math.vector3 renames Float_Math.max;
  function clamp(x  : Float_Math.vector3; a,b : float) return Float_Math.vector3 renames Float_Math.clamp;
  function clamp(x,a,b : Float_Math.vector3) return Float_Math.vector3 renames Float_Math.clamp;

  function transpose(m : Float_Math.Matrix4) return Float_Math.Matrix4 renames Float_Math.transpose;

  function RotationMatrix(angle : float; a_v : float3) return float4x4;
  function TranslationMatrix(a_v : float3) return float4x4;
  function LookAtMatrix(eye, center, up : float3) return float4x4;

  function "*"(m : float4x4; v : float3) return float3;
  function "*"(m : float4x4; v : float4) return float3;
  function "*"(M1 : float4x4; M2 : float4x4) return float4x4;


  IdentityMatrix : constant float4x4 := ((1.0,0.0,0.0,0.0),
                                         (0.0,1.0,0.0,0.0),
                                         (0.0,0.0,1.0,0.0),
                                         (0.0,0.0,0.0,1.0));

  -- integer math
  --
  package Integer_Math is new Generic_Vector_Math (integer);

  type int3 is new Integer_Math.vector3;
  type int4 is new Integer_Math.vector4;

  --function sqr(x : integer) return float renames Integer_Math.sqr;
  function min(a, b : integer) return integer renames Integer_Math.min;
  function max(a, b : integer) return integer renames Integer_Math.max;
  function min(a, b, c : integer) return integer renames Integer_Math.min;
  function max(a, b, c : integer) return integer renames Integer_Math.max;
  function clamp(x,a,b : integer) return integer renames Integer_Math.clamp;

  pragma Inline (normalize);
  pragma Inline (length);
  pragma Inline (reflect);
  pragma Inline (sqr);
  pragma Inline (min);
  pragma Inline (max);
  pragma Inline (clamp);

end Vector_Math;
