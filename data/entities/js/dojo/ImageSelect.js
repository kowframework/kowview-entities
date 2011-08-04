
/**
 * kowview.entities.ImageSelect
 * TODO :: move this to Teca application as it depends in it's image list service
 * and rename it acordingly
 */

if(!dojo._hasResource["kowview.entities.ImageSelect"]){
	dojo._hasResource["kowview.entities.ImageSelect"]=true;
	dojo.provide("kowview.entities.ImageSelect");
	dojo.require("dijit._editor._Plugin");
	dojo.require("dijit.form.Button");
	dojo.require("dojox.editor.plugins.Smiley");
	dojo.declare(
			"kowview.entities.ImageSelect",
			dijit._editor._Plugin,
			{
				// summary:
				// 		this plugin show a simple image selection dialog based on the teca image backend

				_initDialog	: function() {
						// sumary:
						// 	create the dialog used when selecting images
						
						var strings = dojo.i18n.getLocalization("dijit._editor", "commands");
						this.selectImageDialog = new dijit.Dialog({
								title	: "1) " + strings["insertImage"],
								content	: "Por favor, selecione uma imagem abaixo:<br/><ul class=\"imageListing\"></ul>",
								style	: "width: 500px; height: 300px;"
							});
	
						this.selectPositionDialog = new dijit.Dialog( {
								title	: "2) " + strings["insertImage"],
								content	: "Por favor, selecione uma das opções para posicionamento da imagem abaixo:<br/><ul></ul>",
								style	: "width: 500px; height: 300px;"
							});
	
						this.selectSizeDialog = new dijit.Dialog( {
								title	: "3) " + strings["insertImage"],
							  	content	: "Agora basta selecionar o tamanho da imagem:<br/><ul></ul>",
								style	: "width: 500px; height: 300px;"
							});
					},
	
				_initButton	: function(){
						var strings = dojo.i18n.getLocalization("dijit._editor", "commands"),
						editor = this.editor;
						this.button = new dijit.form.Button({
								label		: strings["insertImage"],
								dir		: editor.dir,
								lang		: editor.lang,
								showLabel	: false,
								iconClass	: this.iconClassPrefix + " " + this.iconClassPrefix + "InsertImage",
								tabIndex	: "-1",
								onClick		: dojo.hitch(this, "_selectImage")
							});
					},

				setEditor	: function(/*dijit.Editor*/ editor){
						this.editor = editor;
						this._initDialog();
						this._initButton();
	
						theGlobalImageSelectPlugin = this;
					},

				
				_buildImageList	: function() {
						list = dojo.query( "ul", this.selectImageDialog.containerNode)[0];
						console.dir( list );
						dojo.empty(list);
	
						thePlugin = this;
						dojo.forEach( this.imagesArray, function( item, index ) {
									li = dojo.create( "li", null, list ); 
									a = dojo.create( "a", { href : "javascript:theGlobalImageSelectPlugin._selectPosition("+ index +");"}, li );
									img = dojo.create( "img", { src	: item.url }, a );
								} );
						//this.selectImageDialog.setContent( "referencias ok" );
					},


				_buildPositionList	: function( index ) {
						function s( value, label ) {
							return { value : value, label : label };
						}

						positions = [
								s( "Here", "Onde está posicionada" ),
								s( "Top", "No começo da página" ),
								s( "Bottom", "No final da página" ),
								s( "NewPage", "Em uma nova página" )
							];

						list = dojo.query( "ul", this.selectPositionDialog.containerNode )[0];
						dojo.empty( list );
						dojo.forEach( positions, function(position) {
								li = dojo.create( "li", null, list );
								a  = dojo.create(
										"a",
										{
											href		: "javascript:theGlobalImageSelectPlugin._selectSize(" + index + ", \'" + position.value + "\');",
											innerHTML	: position.label
										},
										li
									);
							});
					},


				_buildSizeList		: function( index, position ) {
						function s( value, label ) {
							return { value : value, label : label };
						}

						sizes = [
								s( "Big", "O maior possível" ),
								s( "Medium", "Médio" ),
								s( "Small", "Pequeno" )
							];

						list = dojo.query( "ul", this.selectSizeDialog.containerNode )[0];
						dojo.empty( list );
						dojo.forEach( sizes, function( size ) {
								li = dojo.create( "li", null, list );
								a  = dojo.create(
										"a",
										{
											href		: "javascript:theGlobalImageSelectPlugin._insertImage(" + index + ", \'" + position + "\', \'" + size.value + "\');",
											innerHTML	: size.label
										},
										li
									);
							});
					},



				_selectImage	: function() {
						this.selectPositionDialog.hide();
						this.selectSizeDialog.hide();
						// show the image selector
						if( !kowview.isSet( this.imagesArray ) ) {
							plugin = this;
							kowview.services.postJson(
									"teca",
									"image_list",
									{
										editor	: this.editor,
										load	: function(data) {
												plugin.imagesArray = data.response.images;
												plugin._buildImageList();
												plugin.selectImageDialog.show();
											}
									}
								);
						} else {
							this.selectImageDialog.show();
						}
				
					},

				_selectPosition	: function( index ) {
						this.selectImageDialog.hide();
						this.selectSizeDialog.hide();
						this._buildPositionList( index );
						this.selectPositionDialog.show();
					},

				_selectSize	: function( index, position ) {
						this.selectImageDialog.hide();
						this.selectPositionDialog.hide();
						this._buildSizeList( index, position );
						this.selectSizeDialog.show();
					},

				_insertImage	: function( index, position, size ) {
						this.selectImageDialog.hide();
						this.selectPositionDialog.hide();
						this.selectSizeDialog.hide();

						img = this.imagesArray[index];
						console.log("will insert:" );
						console.dir(img);


						if( !kowview.isSet( position ) )
							position = "Here";
						if( !kowview.isSet( size ) )
							size = "Normal"
						imgClass = position + ' ' + size;

						this.editor.beginEditing();
						this.editor.execCommand('inserthtml', "<img id=\"image:"+ img.id + "\" class=\"" + imgClass + "\" src=\""+img.url+"\"/>" );
						//this.editor.set("value", this.content);
						this.editor.endEditing();
						this.editor.focus();
					}
			} // properties

	); // declare
} // if not declared



