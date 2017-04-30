module AUCSP.Agent.Run.Data where

import AUCSP.Agent.NegotiatingAgent


data NegotiationDef a = NegotiationDef{
    defDisciplines :: [Discipline]
  , defClassrooms  :: [Classroom]
  , defGroups      :: [RequiredData Group a]
  , defProfessors  :: [RequiredData Professor a]
  , defTimetable   :: SomeDiscreteTimeDescriptor
  }

-- negotiationData = NegotiationDef{
--   defDisciplines = []
-- }
