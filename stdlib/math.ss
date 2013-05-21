@visibility("public")

namespace std
{
 function<float> sqrt(float) [library="math.ssm"];
 function<float> sin(float) [library="math.ssm"];
 function<float> cos(float) [library="math.ssm"];
 function<float> log(float, float) [library="math.ssm"];
 function<float> ln(float) [library="math.ssm"];
 function<float> exp(float) [library="math.ssm"];
 function<float> log10(float) [library="math.ssm"];
 function<float> log2(float) [library="math.ssm"];
 function<int> iabs(int) [library="math.ssm"];
 function<float> fabs(float) [library="math.ssm"];
 function<float> power(float, int) [library="math.ssm"];
 function<int> ipower(int, int) [library="math.ssm"];
 function<bool> is_odd(int) [library="math.ssm"];
 function<bool> is_even(int) [library="math.ssm"];
 function<int> round(float) [library="math.ssm"];
 function<float> fround(float) [library="math.ssm"];
 function<int> round_up(float) [library="math.ssm"];
 function<int> round_down(float) [library="math.ssm"];
 function<int> round_trunc(float) [library="math.ssm"];
 function<float> round_to(float, int) [library="math.ssm"];
 function<int> max(int, int) [library="math.ssm"];
 function<float> fmax(float, float) [library="math.ssm"];
 function<int> min(int, int) [library="math.ssm"];
 function<float> fmin(float, float) [library="math.ssm"];
 function<int> gcd(int, int) [library="math.ssm"];
 function<int> factorial(int) [library="math.ssm"];
 function<bool> in_range(float, float, float) [library="math.ssm"];
}
