{application, 'upload', [
	{description, "Cowboy multipart upload example"},
	{vsn, "1"},
	{modules, ['connector','flowMeterInst','flowMeterTyp','fluidumInst','fluidumTyp','getFlow','getPump','heatExchangeLink','heatExchangerInst','heatExchangerTyp','location','msg','pipeInst','pipeTyp','pipes','pumpInst','pumpTyp','resource_instance','resource_type','setPump','survivor','upload_app','upload_h','upload_sup']},
	{registered, [upload_sup]},
	{applications, [kernel,stdlib,cowboy]},
	{mod, {upload_app, []}},
	{env, []}
]}.