@visibility("public")

const<float> EPSILON = 0.00001;

function<float> fabs(float f)
{
 if (f < 0)
  return -f; else
  return f;
}

function<bool> f_equal(float a, float b)
{
 return fabs(a-b) < EPSILON;
}

function<bool> f_equal_eps(float a, float b, float eps)
{
 return fabs(a-b) < eps;
}

function<bool> f_diff(float a, float b)
{
 return !f_equal(a, b);
}

function<bool> f_diff_eps(float a, float b, float eps)
{
 return !f_equal_eps(a, b, eps);
}
