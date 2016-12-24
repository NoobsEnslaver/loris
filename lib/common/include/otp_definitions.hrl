-ifndef(OTP_DEFINITIONS_HRL).
-define(OTP_DEFINITIONS_HRL, true).

-define(WORKER(Id), #{id => Id, start => {Id, start_link, []}, type => 'worker'}).
-define(WORKER(Id, M), #{id => Id, start => {M, start_link, []}, type => 'worker', modules=>[M]}).
-define(WORKER(Id, M, F), #{id => Id, start => {M, F, []}, type => 'worker', modules=>[M]}).
-define(WORKER(Id, M, F, A), #{id => Id, start => {M, F, A}, type => 'worker', modules=>[M]}).
-define(WORKER(Id, M, F, A, RS), #{id => Id, start => {M, F, A}, type => 'worker', modules=>[M], restart => RS}).

-define(SUPER(Id), #{id => Id, start => {Id, start_link, []}, type => 'supervisor', modules=>[Id]}).
-define(SUPER(Id, RS), #{id => Id, start => {Id, start_link, []}, type => 'supervisor', modules=>[Id], restart => RS}).
-define(SUPER(Id, Args, RS), #{id => Id, start => {Id, start_link, Args}, type => 'supervisor', modules=>[Id], restart => RS}).

-endif.
