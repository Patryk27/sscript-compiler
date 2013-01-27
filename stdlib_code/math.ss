/*
 SScript Standard Library
 math.ss
*/

@visibility("public")

/* ===== ICALL functions ===== */

function<float> sqrt(float value) naked
{
 :CODE
 {
  icall("math.sqrt")
  pop(ef1)
 }
}

function<float> sin(float value) naked
{
 :CODE
 {
  icall("math.sin")
  pop(ef1)
 }
}

function<float> cos(float value) naked
{
 :CODE
 {
  icall("math.cos")
  pop(ef1)
 }
}

function<float> log(float n, float x) naked
{
 :CODE
 {
  icall("math.log")
  pop(ef1)
 }
}

function<float> ln(float x) naked
{
 :CODE
 {
  icall("math.ln")
  pop(ef1)
 }
}

function<float> exp(float value) naked
{
 :CODE
 {
  icall("math.exp")
  pop(ef1)
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

function<float> fabs(float f)
{
 if (f < 0)
  return -f; else
  return f;
}

function<int> iabs(int i)
{
 if (i < 0)
  return -i; else
  return i;
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
 return int(power(base, exp));
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
 :CODE pop(ei1)
}

function<float> fround(float f) naked
{
 :CODE
 {
  pop(ei1)
  mov(ef1, ei1)
 }
}

function<int> round_up(float f) naked
{
 :CODE
 {
  pop(ef1)
  add(ef1, 0.5)
  mov(ei1, ef1)
 }
}

function<int> round_down(float f) naked
{
 :CODE
 {
  pop(ef1)
  sub(ef1, 0.5)
  mov(ei1, ef1)
 }
}

function<int> round_trunc(float x)
{
 var<float> tmp = (x*10);
 return int(tmp)/10;
}

function<float> round_to(float x, int digit)
{
 var<int> fact = ipower(10, digit);
 return fround(x*fact)/fact;
}
