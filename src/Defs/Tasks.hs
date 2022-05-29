module Defs.Tasks where

data TaskStatus = Inactive | Active | Completed deriving (Eq)
data TaskName = TaskTrial | TaskTalkUebe | TaskKillBadGuys | TaskAttackUebe | TaskFindWallet deriving(Eq)
type TaskDesc = String
data Task = Task 
    {
        task_name :: TaskName,
        task_desc :: TaskDesc
    } deriving(Eq)

taskTrial = Task TaskTrial "przejdź próbę w jaskini Próby"

taskTalkUebe = Task TaskTalkUebe "porozmawiaj z Uebe"

taskKillBadGuys = Task TaskKillBadGuys "zabij kłusowników"

taskAttackUebe = Task TaskAttackUebe "zaatakuj Uebe zaklęciem Potassium"

taskFindWallet = Task TaskFindWallet "odzyskaj portfel Uebe z jaskini Kobry"
