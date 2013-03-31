/*
 SScript Standard Library
 time.ss
*/

@visibility("public")

namespace std
{

function<int> get_ticks() naked
{
 :CODE
 {
  icall("time.get_tick_count")
  pop(ei1)
 }
}

function<void> sleep(int milis) naked
{
/*
 finish_time = get_ticks()+milis;

 while (get_ticks() < finish_time) ; // (do nothing)
*/

 :CODE
 {
  sub(stp, 1)

  pop(ei1) // ei1 = milis

  icall("time.get_tick_count")
  pop(ei2) // ei2 = current time

  add(ei2, ei1) // ei2 = finish time

 &loop:
  icall("time.get_tick_count")
  pop(ei1) // ei1 = current time

  if_l(ei1, ei2) // if (ei1 < ei2)
  tjmp(:&loop)   //  goto loop;

  add(stp, 2)
 }
}

}