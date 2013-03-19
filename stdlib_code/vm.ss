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

}