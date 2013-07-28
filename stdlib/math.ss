@visibility("public")

namespace std
{
 function<float> cos(float) [library="math.ssm"];
 function<float> sin(float) [library="math.ssm"];
 function<float> tan(float) [library="math.ssm"];
 function<float> acos(float) [library="math.ssm"];
 function<float> asin(float) [library="math.ssm"];
 function<float> atan(float) [library="math.ssm"];
 function<float> atan2(float, float) [library="math.ssm"];
 function<float> cosh(float) [library="math.ssm"];
 function<float> sinh(float) [library="math.ssm"];
 function<float> tanh(float) [library="math.ssm"];
 function<float> acosh(float) [library="math.ssm"];
 function<float> asinh(float) [library="math.ssm"];
 function<float> atanh(float) [library="math.ssm"];
 function<float> exp(float) [library="math.ssm"];
 function<float> log(float, float) [library="math.ssm"];
 function<float> log10(float) [library="math.ssm"];
 function<float> log2(float) [library="math.ssm"];
 function<float> ln(float) [library="math.ssm"];
 function<float> power(float, int) [library="math.ssm"];
 function<int> ipower(int, int) [library="math.ssm"];
 function<float> sqrt(float) [library="math.ssm"];
 function<float> hypot(float, float) [library="math.ssm"];
 function<int> round(float) [library="math.ssm"];
 function<int> round_up(float) [library="math.ssm"];
 function<int> round_down(float) [library="math.ssm"];
 function<int> round_trunc(float) [library="math.ssm"];
 function<float> round_to(float, int) [library="math.ssm"];
 function<int> imax(int, int) [library="math.ssm"];
 function<float> fmax(float, float) [library="math.ssm"];
 function<int> imin(int, int) [library="math.ssm"];
 function<float> fmin(float, float) [library="math.ssm"];
 function<int> factorial(int) [library="math.ssm"];
 function<int> iabs(int) [library="math.ssm"];
 function<float> fabs(float) [library="math.ssm"];
 function<bool> is_even(int) [library="math.ssm"];
 function<bool> is_odd(int) [library="math.ssm"];
 function<int> gcd(int, int) [library="math.ssm"];
 function<bool> in_range(float, float, float) [library="math.ssm"];
 function<void> modf(float, var int, var float) [library="math.ssm"];
}
