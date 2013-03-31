@visibility("public")

namespace std
{
 const EPSILON = 0.00001;
 function<float> fabs(float) [library="float.ssm"];
 function<bool> f_equal(float, float) [library="float.ssm"];
 function<bool> f_equal_eps(float, float, float) [library="float.ssm"];
 function<bool> f_diff(float, float) [library="float.ssm"];
 function<bool> f_diff_eps(float, float, float) [library="float.ssm"];
}
