



with Aw_View;

generic
	type Entity_Type is new Aw_Ent.Entity_Type with private;
	Component_Name	: constant String;
	Template_Name	: constant String;
package Aw_View.Forms is


	type Component_Type is new Aw_View.Component_Type;


	-- módulos:
	-- 	edit_form
	-- 	insert_form
	--	display_form
	--	list_elements_form
	--	(...)
	--
	--
	--	Serviços? AJAX!



end Aw_View.Forms;
