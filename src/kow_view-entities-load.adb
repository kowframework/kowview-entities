with KOW_View.Components_Registry;


procedure KOW_View.Entities.Load is
begin

	
	KOW_View.Components_Registry.Register(
		"entities",
		new KOW_View.Entities.Component_Type,
		false
	);



end KOW_View.Entities.Load;
