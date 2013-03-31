@visibility("public")

namespace std
{
 function<int> get_ticks() [library="time.ssm"];
 function<void> sleep(int) [library="time.ssm"];
}
