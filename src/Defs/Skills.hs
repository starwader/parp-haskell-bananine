module Defs.Skills where

data SkillType
  = SkillPotassiumSpell
  | SkillTiuFiu
  deriving (Eq)

type SkillDesc = String

data Skill = Skill
  { skill_type :: SkillType,
    skill_name :: SkillDesc
  }
  deriving (Eq)

-- wszystkie umiejętności w grze
skillPotassiumSpell = Skill SkillPotassiumSpell "Zaklęcie Potassium"
skillTiuFiu = Skill SkillTiuFiu "Tiu-fiu"