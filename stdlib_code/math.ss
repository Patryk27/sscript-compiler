/*
 SScript Standard Library
 math.ss
*/

@visibility("public")

namespace std
{

/* ===== `icall` functions ===== */

function<float> sqrt(float value) naked
{
 :CODE
 {
  sub(stp, 1)
  icall("math.sqrt")
  pop(ef1)
  add(stp, 2)
 }
}

function<float> sin(float value) naked
{
 :CODE
 {
  sub(stp, 1)
  icall("math.sin")
  pop(ef1)
  add(stp, 2)
 }
}

function<float> cos(float value) naked
{
 :CODE
 {
  sub(stp, 1)
  icall("math.cos")
  pop(ef1)
  add(stp, 2)
 }
}

function<float> log(float n, float x) naked
{
 :CODE
 {
  sub(stp, 1)
  icall("math.log")
  pop(ef1)
  add(stp, 2)
 }
}

function<float> ln(float x) naked
{
 :CODE
 {
  sub(stp, 1)
  icall("math.ln")
  pop(ef1)
  add(stp, 2)
 }
}

function<float> exp(float value) naked
{
 :CODE
 {
  sub(stp, 1)
  icall("math.exp")
  pop(ef1)
  add(stp, 2)
 }
}

/* ===== regular functions ===== */

function<float> log10(float n)
{
 return log(10, n);
}

function<float> log2(float n)
{
 return log(2, n);
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

function<float> power(float base, int exp) /* fast power algorithm (non-recursive version) */
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
 return cast<int>(power(base, exp));
}

function<bool> is_odd(int n)
{
 return (n&1); // (n%2) != 1
}

function<bool> is_even(int n)
{
 return !(n&1); // (n%2) == 1
}

function<int> round(float f) naked
{
 :CODE
 {
  sub(stp, 1)
  pop(ei1)
  add(stp, 2)
 }
}

function<float> fround(float f) naked
{
 :CODE
 {
  mov(ei1, [-1])
  mov(ef1, ei1)
  add(stp, 2)
 }
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

function<int> round_trunc(float x)
{
 var<float> tmp = (x*10);
 return cast<int>(tmp)/10;
}

function<float> round_to(float x, int digit)
{
 var<int> fact = ipower(10, digit);
 return fround(x*fact)/fact;
}

function<int> max(int a, int b)
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

function<int> min(int a, int b)
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

/* http://en.wikipedia.org/wiki/Binary_GCD_algorithm#Iterative_version_in_C */
function<int> gcd(int u, int v)
{
 if ((u < 0) || (v < -0))
  return 0;

 var<int> shift;
 
 if (u == 0)
  return v;
  
 if (v == 0)
  return u;
 
 for (shift = 0; ((u | v) & 1) == 0; ++shift)
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

}