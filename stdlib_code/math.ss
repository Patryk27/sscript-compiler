/*
 SScript Standard Library
 math.ss
*/

@visibility("public")

namespace std
{

/* Trigonometric functions */
function<float> cos(float x) naked
{
 :CODE
 {
  push(%x)
  icall("math.cos")
  pop(ef1)
 }
}

function<float> sin(float x) naked
{
 :CODE
 {
  push(%x)
  icall("math.sin")
  pop(ef1)
 }
}

function<float> tan(float x) naked
{
 :CODE
 {
  push(%x)
  icall("math.tan")
  pop(ef1)
 }
}

function<float> acos(float x) naked
{
 :CODE
 {
  push(%x)
  icall("math.acos")
  pop(ef1)
 }
}

function<float> asin(float x) naked
{
 :CODE
 {
  push(%x)
  icall("math.asin")
  pop(ef1)
 }
}

function<float> atan(float x) naked
{
 :CODE
 {
  push(%x)
  icall("math.atan")
  pop(ef1)
 }
}

function<float> atan2(float y, float x) naked
{
 :CODE
 {
  sub(stp, 1)
  icall("math.atan2")
  pop(ef1)
  add(stp, 3)
 }
}

/* Hyperbolic functions */
function<float> cosh(float x) naked
{
 :CODE
 {
  push(%x)
  icall("math.cosh")
  pop(ef1)
 }
}

function<float> sinh(float x) naked
{
 :CODE
 {
  push(%x)
  icall("math.sinh")
  pop(ef1)
 }
}

function<float> tanh(float x) naked
{
 :CODE
 {
  push(%x)
  icall("math.tanh")
  pop(ef1)
 }
}

function<float> acosh(float x) naked
{
 :CODE
 {
  push(%x)
  icall("math.acosh")
  pop(ef1)
 }
}

function<float> asinh(float x) naked
{
 :CODE
 {
  push(%x)
  icall("math.asinh")
  pop(ef1)
 }
}

function<float> atanh(float x) naked
{
 :CODE
 {
  push(%x)
  icall("math.atanh")
  pop(ef1)
 }
}

/* Exponential and logarithmic functions */
function<float> exp(float x) naked
{
 :CODE
 {
  push(%x)
  icall("math.exp")
  pop(ef1)
 }
}

function<float> log(float n, float x) naked
{
 :CODE
 {
  sub(stp, 1)
  icall("math.log")
  pop(ef1)
  add(stp, 3)
 }
}

function<float> log10(float n)
{
 return log(10, n);
}

function<float> log2(float n)
{
 return log(2, n);
}

function<float> ln(float x) naked
{
 :CODE
 {
  push(%x)
  icall("math.ln")
  pop(ef1)
 }
}

/* Power functions */
function<float> power(float base, int exp) // fast power algorithm (non-recursive version)
{
 var<float> res = 1;

 while (exp > 0)
 {
  if (exp&1)
   res *= base;

  base *= base;
  exp >>= 1;
 }

 return res;
}

function<int> ipower(int base, int exp)
{
 return round(power(base, exp));
}

function<float> sqrt(float x) naked
{
 :CODE
 {
  push(%x)
  icall("math.sqrt")
  pop(ef1)
 }
}

function<float> hypot(float x, float y)
{
 return sqrt(x*x + y*y);
}

/* Rounding and remainder functions */
function<int> round(float f) naked
{
 :CODE
  mov(ei1, [-1])
}

function<int> round_up(float f) naked
{
 :CODE
 {
  mov(ef1, [-1])
  add(ef1, 0.5)
  mov(ei1, ef1)
 }
}

function<int> round_down(float f) naked
{
 :CODE
 {
  mov(ef1, [-1])
  sub(ef1, 0.5)
  mov(ei1, ef1)
 }
}

function<int> round_trunc(float x) naked
{
 :CODE
 {
  mul([-1], 10)
  mov(ei1, [-1])
  div(ei1, 10)
 }
}

function<float> round_to(float x, int digit)
{
 var<int> fact = ipower(10, digit);
 return (cast<float>(x*fact))/fact;
}

/* Minimum, maximum, difference functions */
function<int> imax(int a, int b)
{
 if (a > b)
  return a; else
  return b;
}

function<float> fmax(float a, float b)
{
 if (a > b)
  return a; else
  return b;
}

function<int> imin(int a, int b)
{
 if (a < b)
  return a; else
  return b;
}

function<float> fmin(float a, float b)
{
 if (a < b)
  return a; else
  return b;
}

/* Other functions */
function<int> factorial(int num)
{
 var<int> result = 1;

 while (--num > 0)
  result *= num+1;

 return result;
} 

function<int> iabs(int i)
{
 if (i < 0)
  return -i; else
  return i;
}

function<float> fabs(float f)
{
 if (f < 0)
  return -f; else
  return f;
}

function<bool> is_even(int n)
{
 return !(n&1); // (n%2) == 0
}

function<bool> is_odd(int n)
{
 return (n&1); // (n%2) != 0
}

/* http://en.wikipedia.org/wiki/Binary_GCD_algorithm#Iterative_version_in_C */
function<int> gcd(int u, int v)
{
 if ((u < 0) || (v < 0))
  return 0;

 var<int> shift;
 
 if (u == 0)
  return v;
  
 if (v == 0)
  return u;
 
 for (shift = 0; ((u | v) & 1) == 0; shift++)
 {
  u >>= 1;
  v >>= 1;
 }
 
 while ((u & 1) == 0)
  u >>= 1;
 
 do
 { 
  while ((v & 1) == 0)
   v >>= 1;

  if (u > v)
  {
   var<int> t = v;
   v = u;
   u = t;
  } 
       
  v-= u;
 } while (v != 0);

 return u << shift;
}

function<bool> in_range(float num, float lower, float upper)
{
 return (num >= lower) && (num <= upper);
}

function<void> modf(float number, var int int_part, var float float_part)
{
 int_part   = round_trunc(number);
 float_part = number-int_part;
}
}