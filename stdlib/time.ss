@visibility("public")

namespace std
{
 function<int> get_milliseconds() [library="time.ssm"];
 function<int> get_millis() [library="time.ssm"];
 function<void> sleep(int) [library="time.ssm"];
}
