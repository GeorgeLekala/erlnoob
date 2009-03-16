%%%-------------------------------------------------------------------
%%% File    : ex1_parser.hrl
%%% Author  : Olle Mattsson <olle@zubat>
%%% Description : 
%%%
%%% Created : 13 Mar 2009 by Olle Mattsson <olle@zubat>
%%%-------------------------------------------------------------------

-record(object_ref, {name,
		     mats}).


-record(material, {name,
		   diffuse,
		   ambient,
		   specular,
		   emission,
		   shininess,
		   textures}).
