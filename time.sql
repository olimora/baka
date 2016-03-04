SELECT to_timestamp('2015-03-29 05','YYYY-MM-DD HH24') 
at time zone 'UTC'

select timestamp with time zone '2015-03-28 05 +01' at time zone 'UTC'

create table test_time (
	id serial,
	cas1 timestamp without time zone,
	cas2 timestamp with time zone,
	cas3 timestamp
)
drop table test_time
delete from test_time


insert into test_time (cas1, cas2, cas3) VALUES 
(to_timestamp('2015-03-29 10','YYYY-MM-DD HH24'),
to_timestamp('2015-03-29 10','YYYY-MM-DD HH24'),
to_timestamp('2015-03-29 10','YYYY-MM-DD HH24')),
(to_timestamp('2015-03-28 10','YYYY-MM-DD HH24'),
to_timestamp('2015-03-28 10','YYYY-MM-DD HH24'),
to_timestamp('2015-03-28 10','YYYY-MM-DD HH24'));

select 	cas1 c1, cas1 at time zone 'CET' c1UTC, 
	cas2 c2, cas2 at time zone 'CET' c2UTC, 
	cas3 c3, cas3 at time zone 'CET' c3UTC
from test_time


show timezone
set timezone='UTC'
set timezone='BST';
--------------------------------
delete from test_time;
set timezone='UTC';

insert into test_time (cas1, cas2, cas3) VALUES 
(to_timestamp('2015-03-29 10','YYYY-MM-DD HH24'),
to_timestamp('2015-03-29 10','YYYY-MM-DD HH24'),
to_timestamp('2015-03-29 10','YYYY-MM-DD HH24')),
(to_timestamp('2015-03-28 10','YYYY-MM-DD HH24'),
to_timestamp('2015-03-28 10','YYYY-MM-DD HH24'),
to_timestamp('2015-03-28 10','YYYY-MM-DD HH24'));

set timezone='CET';

insert into test_time (cas1, cas2, cas3) VALUES 
(to_timestamp('2015-03-29 10','YYYY-MM-DD HH24'),
to_timestamp('2015-03-29 10','YYYY-MM-DD HH24'),
to_timestamp('2015-03-29 10','YYYY-MM-DD HH24')),
(to_timestamp('2015-03-28 10','YYYY-MM-DD HH24'),
to_timestamp('2015-03-28 10','YYYY-MM-DD HH24'),
to_timestamp('2015-03-28 10','YYYY-MM-DD HH24'));

set timezone='UTC';
select 	cas1 c1, cas1 at time zone 'UTC' c1UTC, 
	cas2 c2, cas2 at time zone 'UTC' c2UTC, 
	cas3 c3, cas3 at time zone 'UTC' c3UTC
from test_time;

select to_timestamp('2015-03-29 10:00 CET', 'YYYY-MM-DD HH24:MI TZ')  