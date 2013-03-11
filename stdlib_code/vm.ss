/*
 SScript Standard Library
 vm.ss
*/

@visibility("public")

namespace std
{

function<void> exit(int exitcode) naked
{
 :CODE
 {
  sub(stp, 1)
  icall("vm.exit")
  stop() // shouldn't happen
 }
}

function<void> throwError(string msg) naked
{
 :CODE
 {
  sub(stp, 1)
  icall("vm.throw")
  add(stp, 2)
 }
}

function<string> getLastError() naked
{
 :CODE
 {
  icall("vm.get_last_exception")
  pop(es1)
 }
}

}