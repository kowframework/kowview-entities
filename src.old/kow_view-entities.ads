------------------------------------------------------------------------------
--                                                                          --
--                          KOW Framework :: View                           --
--                                                                          --
--                              KOW Framework                               --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--               Copyright (C) 2007-2011, KOW Framework Project             --
--                                                                          --
--                                                                          --
-- KOWView is free software; you can redistribute it  and/or modify it under--
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion. KOWView is distributed in the hope that it will be useful,but WITH---
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License distributed with KOWView; see file COPYING.  If not, write--
-- to  the Free Software Foundation,  59 Temple Place - Suite 330,  Boston, --
-- MA 02111-1307, USA.                                                      --
--                                                                          --
------------------------------------------------------------------------------
pragma License( GPL );




--------------
-- Ada 2005 --
--------------
with Ada.Containers.Ordered_Maps;
with Ada.Strings.Unbounded;		use Ada.Strings.Unbounded;
with Ada.Tags;

-------------------
-- KOW Framework --
-------------------
with KOW_Config;
with KOW_Ent;
with KOW_Lib.File_System;
with KOW_Lib.UString_Vectors;
with KOW_View.Components;

---------
-- AWS --
---------
with AWS.Response;
with AWS.Status;
with Templates_Parser;

package KOW_View.Entities is




	type Service_Triggering_Entity_Type is interface;
	-- entity type called by the store_entity_service (that can be use to retrieve username, for instance)

	procedure Before_Service(
			Entity		: in out Service_Triggering_Entity_Type;
			Service		: in out KOW_View.Components.Service_Instance_Interface'Class;
			Request		: in     AWS.Status.Data
		) is abstract;

	procedure After_Service(
			Entity		: in out Service_Triggering_Entity_Type;
			Service		: in out KOW_View.Components.Service_Instance_Interface'Class;
			Request		: in     AWS.Status.Data;
			Response	: in out AWS.Response.Data
		) is abstract;




	--------------------------
	-- Store Entity Service --
	--------------------------

	type Store_Entity_Service is new KOW_View.Components.Service_Instance_Interface with private;


	overriding
	procedure Process_Request(
			Service		: in out Store_Entity_Service;
			Request		: in     AWS.Status.Data;
			Response	: in out AWS.Response.Data
		);
	-- read the entity from the form and process it.
	-- save it back to the database backend afterwards


	---------------------------
	-- File Download Service --
	---------------------------


	type File_Download_Service is new KOW_View.Components.Service_Instance_Interface with null record;

	overriding
	procedure Process_Request(
			Service		: in out File_Download_Service;
			Request		: in     AWS.Status.Data;
			Response	: in out AWS.Response.Data
		);
	-- get an uploaded file, respecting the URL:
	-- 	/service_mapping/[entity_tag]/[entity_id]/[column_name]
	-- serve the file using the name used in the storage


	----------------------------
	-- Image Download Service --
	----------------------------

	type Image_Download_Service is new File_Download_Service with null record;

	overriding
	procedure Process_Request(
			Service		: in out Image_Download_Service;
			Request		: in     AWS.Status.Data;
			Response	: in out AWS.Response.Data
		);
	-- same as the file download service, but with the option of showing the thumbnail..



	type Get_IDs_Function_Access is access function(
					Module	: in Entity_Browser_Module'Class;
					Filter	: in String;
					Request	: in AWS.Status.Data
			) return KOW_Ent.Id_Array_Type;
	
	package Get_IDs_Function_Maps is new Ada.Containers.Ordered_Maps(
						Key_Type	=> Unbounded_String,
						Element_Type	=> Get_IDs_Function_Access
					);

	protected Get_IDs_Functions_Registry is
		function Get( Name : in Unbounded_String ) return Get_IDs_Function_Access;
		procedure Register( Name : in String; Function_Access : in Get_IDs_Function_Access );
	private
		My_Map : Get_IDs_Function_Maps.Map;
	end Get_IDs_Functions_Registry;



	---------------------------------------
	-- Services for the Entity component --
	---------------------------------------

	type Store_Entity_Service is new KOW_View.Components.Service_Instance_Interface with record
		Variable_Prefix 	: String( 1 .. 6  ) := "entity";
		-- the prefix for every variable to be processed by this service
		Inlined_Variable_Prefix	: String( 1 .. 14 ) := "inlined_entity";
	end record;





end KOW_View.Entities;
