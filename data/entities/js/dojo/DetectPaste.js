
/**
 * kowview.entities.DetectPaste
 */

if(!dojo._hasResource["kowview.entities.DetectPaste"]){
	dojo._hasResource["kowview.entities.DetectPaste"]=true;
	dojo.provide("kowview.entities.DetectPaste");
	dojo.require("dijit._editor._Plugin");
	dojo.declare(
			"kowview.entities.DetectPaste",
			dijit._editor._Plugin,

				{
					blockedTags:	new Array( "span", "meta", "font" ),

					setEditor: function(/*dijit.Editor*/ editor){
							// summary:
							//		Tell the plugin which Editor it is associated with.
							// editor: Object
							//		The editor object to attach the newPage capability to.
							this.editor = editor;

							this.editor.lastValue = this.editor.getValue();
							dojo.connect(
									editor,
									"onKeyDown",
									this,
									this._detectPaste
								);

							dojo.connect(
									editor,
									"onKeyUp",
									this,
									this._detectPaste
								);

							dojo.connect(
									editor,
									"onChange",
									this,
									this._detectPaste
								);

							dojo.connect(
									editor,
									"onClick",
									this,
									this._detectPaste
								);

							dojo.connect(
									editor,
									"execCommand",
									this,
									this._syncValue
								);

							dojo.connect(
									editor,
									"setValue",
									this,
									this._syncValue
								);
							this._syncValue();

						},


					_detectPaste	: function() {
						val = this.editor.getValue();
						if( this.editor.lastValue.length + 25 < val.length )
							this._pasteDetected();

						for( i = 0; i < this.blockedTags.length; i++ ) {
							if( val.indexOf( this.blockedTags[i] ) > 0 )
								this._pasteDetected();
						};

						this._syncValue();
					},

					_pasteDetected	: function() {
							kowview.showErrorMessage( "Colar reconhecido!", "Você efetuou uma operação de colar texto. Isso não é permitido no Teca" );
							this.editor.inputField.value = this.editor.lastValue;
							this.editor.setValue( this.editor.lastValue );
						
							throw "PasteDetected";
						},

					_syncValue	: function() {
							this.editor.lastValue = this.editor.getValue();
						},
					_newPage: function(){
						// summary:
						//		Function to set the content to blank.
						// tags:
						//		private
						//
						this.editor.beginEditing();
						this.editor.execCommand('inserthtml', this.content );
						//this.editor.set("value", this.content);
						this.editor.endEditing();
						this.editor.focus();
					}
				}


			);
}


