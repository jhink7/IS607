-- Database: "CUNY607"

-- DROP DATABASE "CUNY607";

CREATE DATABASE "CUNY607"
  WITH OWNER = postgres
       ENCODING = 'UTF8'
       TABLESPACE = pg_default
       LC_COLLATE = 'English_Canada.1252'
       LC_CTYPE = 'English_Canada.1252'
       CONNECTION LIMIT = -1;

-- Table: pbpraw

-- DROP TABLE pbpraw;

CREATE TABLE pbpraw
(
  season character varying(255),
  gcode character varying(255),
  refdate double precision,
  event integer,
  period double precision,
  seconds double precision,
  etype character varying(255),
  a1 character varying(255),
  a2 character varying(255),
  a3 character varying(255),
  a4 character varying(255),
  a5 character varying(255),
  a6 character varying(255),
  h1 character varying(255),
  h2 character varying(255),
  h3 character varying(255),
  h4 character varying(255),
  h5 character varying(255),
  h6 character varying(255),
  evteam character varying(255),
  evplayer1 character varying(255),
  evplayer2 character varying(255),
  evplayer3 character varying(255),
  distance double precision,
  type character varying(255),
  homezone character varying(255),
  xcoord double precision,
  ycoord double precision,
  awayteam character varying(255),
  hometeam character varying(255),
  homescore double precision,
  awayscore double precision,
  eventlength double precision,
  awayg character varying(255),
  homeg character varying(255),
  homeskaters double precision,
  awayskaters double precision,
  ishomeshevent double precision,
  isawayshevent double precision,
  homecummulative double precision,
  awaycummulative double precision
)
WITH (
  OIDS=FALSE
);
ALTER TABLE pbpraw
  OWNER TO "Justin";

