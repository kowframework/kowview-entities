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
with KOW_Lib.Json;
with KOW_Lib.File_System;
with KOW_Lib.UString_Vectors;
with KOW_Sec.Accounting;
with KOW_View.Services;

---------
-- AWS --
---------
with AWS.Status;

package KOW_View.Entities is



	Accountant      : aliased KOW_Sec.Accounting.Accountant_Type := KOW_Sec.Accounting.New_Accountant( "entities", KOW_View.Accountant'Access );


	-----------------------------------------
	-- Service Triggering Entity Interface --
	-----------------------------------------

	type Service_Triggering_Entity_Interface is interface;
	-- implement this interface in your entity if you want all the methods in this component to call
	-- Before_Service and After_Service

	procedure Before_Service(
			Entity		: in out Service_Triggering_Entity_Interface;
			Service		: in out KOW_View.Services.Service_Type'Class;
			Request		: in     AWS.Status.Data
		) is abstract;

	procedure After_Service(
			Entity		: in out Service_Triggering_Entity_Interface;
			Service		: in out KOW_View.Services.Service_Type'Class;
			Request		: in     AWS.Status.Data
		) is abstract;


	

	procedure Before_Service(
			Entity		: in out KOW_Ent.Entity_Type'Class;
			Service		: in out KOW_View.Services.Service_Type'Class;
			Request		: in     AWS.Status.Data
		);
	-- call the service_triggering interface's before_service if available
	
	procedure After_Service(
			Entity		: in out KOW_Ent.Entity_Type'Class;
			Service		: in out KOW_View.Services.Service_Type'Class;
			Request		: in     AWS.Status.Data
		);
	-- call the service_triggering interface's after_service if available


	---------------------
	-- Rendering Style --
	---------------------

	type Rendering_Style_Type is (
				Big_Rendering,
				Small_Rendering,
				Big_Edit_Rendering,
				Small_Edit_Rendering
			);
	-- how the entity can be rendered..



end KOW_View.Entities;
