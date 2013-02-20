@visibility("public")

namespace std
{
 const<float> EPSILON = 0.00001;
 function<float> fabs(float) in "float.ssm";
 function<bool> f_equal(float, float) in "float.ssm";
 function<bool> f_equal_eps(float, float, float) in "float.ssm";
 function<bool> f_diff(float, float) in "float.ssm";
 function<bool> f_diff_eps(float, float, float) in "float.ssm";
}
