------------------------------------------------------------------------------
--                                                                          --
--                          KOW Framework :: View                           --
--                                                                          --
--                              KOW Framework                               --
--                                                                          --
--                                 B o d y                                  --
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
with Ada.Directories;
with Ada.Tags;
with Ada.Text_IO;	use Ada.Text_IO;

-------------------
-- KOW Framework --
-------------------
with KOW_Ent.ID_Query_Builders;
with KOW_Lib.Log;
with KOW_Lib.String_Util;
with KOW_Lib.UString_Vectors;
with KOW_Sec;
with KOW_View;
with KOW_View.Security;


---------
-- AWS --
---------
with AWS.Status;


package body KOW_View.Entities is





	procedure Before_Service(
			Entity		: in out KOW_Ent.Entity_Type'Class;
			Service		: in out KOW_View.Services.Service_Type'Class;
			Request		: in     AWS.Status.Data
		) is
		-- call the service_triggering interface's before_service if available
	begin
		if Entity in Service_Triggering_Entity_Interface'Class then
			Before_Service(
					Entity	=> Service_Triggering_Entity_Interface'Class( Entity ),
					Service	=> Service,
					Request	=> Request
				);
		end if;
	end Before_Service;
	
	procedure After_Service(
			Entity		: in out KOW_Ent.Entity_Type'Class;
			Service		: in out KOW_View.Services.Service_Type'Class;
			Request		: in     AWS.Status.Data
		) is
		-- call the service_triggering interface's after_service if available
	begin
		if Entity in Service_Triggering_Entity_Interface'Class then
			After_Service(
					Entity	=> Service_Triggering_Entity_Interface'Class( Entity ),
					Service	=> Service,
					Request	=> Request
				);
		end if;
	end After_Service;

end KOW_View.Entities;
