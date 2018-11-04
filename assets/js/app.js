/* Code written by Sumer Kohli - sumer.kohli@berkeley.edu */

hljs.initHighlightingOnLoad();

String.prototype.replaceAll = function(search, replacement) {
	// From https://stackoverflow.com/questions/1144783/how-to-replace-all-occurrences-of-a-string-in-javascript
    var target = this;
    return target.split(search).join(replacement);
};

$(function() {
	// Define DOM helper functions for status updates and the like
	var allBoxes = ['answer', 'answer_lhs', 'answer_rhs'];
	allBoxes.map(x => $('#' + x).addClass('init-outline'));
	var lastBoxTyping = [];
	var prepCodeArea = function() {
		$('code.scheme').each(function(i, block){
			$(block).html("");
		});
	};
	var refreshCode = function() {
		$('#debug_area').show();
		$('code.scheme').each(function(i, block){
			if($(block).text().length == 0){
				$(block).parent().hide();
			} else {
				$(block).parent().show();
				hljs.highlightBlock(block);
			}
		});
	};
	var hideError = function() {
		$('#error_box').hide();
		for(var box of allBoxes){
			$('#' + box).removeClass('syntax-outline').removeClass('success-outline')
			;
		}
	};
	var displayError = function(err) {
		$('#error_message').text(err);
		$('#error_box').show();
		refreshCode();
	};
	var ignoreColors = 0;
	var showSuccessColors = function() {
		if(ignoreColors > 0){
			--ignoreColors;
			return;
		}
		for(var box of lastBoxTyping){
			$('#' + box).removeClass('syntax-outline').addClass('success-outline');
		}
	};
	var showFailureColors = function() {
		for(var box of lastBoxTyping){
			$('#' + box).addClass('syntax-outline').removeClass('success-outline');
		}
	};
	var handleMessage = function(type, data){
		data = data.toString();
		var idx = ["infix-input", "parsed-infix", "simplified-infix", "derivative-prefix", "derivative-infix"];
		if(idx.includes(type)){
			$($('.debug_outputs').get(idx.indexOf(type))).text(data);
		}
	};

	// Instantiate interrpreter
	const GREEK_LETTERS = 'pi theta alpha beta gamma';
	const IMPLEMENTED_FUNCTIONS = 'sin cos tan sec csc cot sinh cosh tanh sech csch coth log ln sqrt abs arcsin arccos arctan arcsec arccsc arccot';
	const CONSTANT_VAR_NAMES = ["i", "e"] + GREEK_LETTERS.split(' ');
	BiwaScheme.define_libfunc("get-implemented-functions", 0, null, function(ar){
		return BiwaScheme.array_to_list(IMPLEMENTED_FUNCTIONS
			.split(' ')
			.map(x => BiwaScheme.Sym(x))
		);
	});
	BiwaScheme.define_libfunc("derivative-dne", 1, 1, function(ar){
		console.warn(ar);
		displayError(ar[0]);
	});
	BiwaScheme.define_libfunc("error", 1, null, function(ar){
		console.error(ar);
		var massageKlass = function(klass){
			if(typeof klass === 'string' || klass instanceof String){
				return klass;
			}
			if('name' in klass){
				return klass.name;
			}
			return klass.toString();
		};
		var err_message = ar.map(x => massageKlass(x));
		displayError(err_message.join(" "));
	});
	BiwaScheme.define_libfunc("pass-message", 2, 3, function(ar){
		if(ar[0].toString().length > 0){
			console.log(ar[0], ar[1].toString());
		}
		if(ar.length == 3){
			handleMessage(ar[2].toString(), ar[1]);
		}
	});
	var biwa = new BiwaScheme.Interpreter(function(e) {
		console.error(e);
		showFailureColors();
	});
	var envLoaded = false;
	var unmangledFnName = "";
	biwa.evaluate("(begin (load \"/assets/scm/diff.scm\") __mangled__derive-infix)", function(result) {
		unmangledFnName = result.toString().slice(1);
		envLoaded = true;
		console.log("Scheme environment loaded");
	});

	// Get latest API version
	var MQ = MathQuill.getInterface(2);

	// Listen for MathJAX startup
	MathJax.Hub.Config({
		// "fast-preview": {
		// 	disabled:true
		// },
		// tex2jax: {
		// 	preview: "none",
		// 	inlineMath: [["$","$"],["\\(","\\)"]]
		// },
		displayAlign: "left",
		CommonHTML: {
			scale: 100,
			linebreaks: {
				width: "83%",
				automatic: true
			}
		},
		"HTML-CSS": { linebreaks: { automatic: true } },
		SVG: { linebreaks: { automatic: true } },
		messageStyle: "none"
	});
	var needsOperatorName = [
		"arcsec",
		"arccsc",
		"arccot"
	];
	MathJax.Hub.Register.StartupHook("AsciiMath Jax Config",function () {
		var AM = MathJax.InputJax.AsciiMath.AM;
		// console.log("Called", AM, AM.TOKEN);
		for(var sym of needsOperatorName){
			AM.newsymbol({input:sym, tag:"mi", output:sym, tex:null, ttype:AM.TOKEN.UNARY, func:true});
		}
	});

	// Define LaTeX to infix ASCII conversion function
	var stripLatexSpaces = function(latex) {
		var out_str = '';
		for(var i = 0; i < latex.length; i++){
			var on = latex[i];
			var next = ((i + 1 < latex.length) ? latex[i + 1] : "");
			if(on == "\\" && next == " "){
				continue;
			}
			if(on == " " && next == " "){
				continue;
			}
			if(on == " " && next == "\\"){
				continue;
			}
			out_str += on;
		}
		while(latex.includes("  ")) latex = latex.replace("  ", " "); // flatten spaces
		return out_str;
	};
	var operators = ["+", "-", "/", "*", "^"];
	var isLetter = function(str) {
		return str.match(/[a-z]/i);
	};
	var isNumber = function(str) {
		return str.match(/[0-9]/i);
	};
	var latex2ascii = function(latex) {
		// Do broad replace-rules.
		if(latex.includes("=")){
			var arr = latex.split("=");
			return latex2ascii(arr[0]) + " = " + latex2ascii(arr[1]);
		}
		latex = latex.slice(0);
		latex = latex.replaceAll("^{ }", "");
		latex = latex.replaceAll("\\cdot", " * ");
		latex = latex.replaceAll("\\left", " ");
		latex = latex.replaceAll("\\right", " ");

		// Do a pass through the string, adding space where necessary
		var out_str = '';
		var frac_stack = [];
		var prev = "";
		for(var i = 0; i < latex.length; i++){
			var on = latex.slice(i, i + 1);
			var next = ((i + 1 < latex.length) ? latex.slice(i + 1, i + 2) : "");
			var next_non_space = next;
			if(next !== ""){
				var j = i + 1;
				while(j < latex.length && latex[j] == " ") ++j;
				next_non_space = latex[j];
				if(!next_non_space || next_non_space === " "){
					next_non_space = "";
				}
			}
			if(on == "\\"){
				// Handle escape sequences specially
				i++;
				var letter = "";
				while(i < latex.length){
					if(isLetter(latex[i])){
						letter += latex[i];
						++i;
					} else break;
				}
				if(letter.length > 0) --i;
				if(["frac", "left", "right"].includes(letter)){
					continue;
				}
				if(letter === "operatorname"){
					while(++i < latex.length){
						if(latex[i] == "\{") break;
					}
					from = i;
					while(++i < latex.length){
						if(latex[i] == "\}"){
							break;
						}
					}
					out_str += latex.slice(from + 1, i);
					continue;
				}
				out_str += " " + letter;
				if(GREEK_LETTERS.includes(letter)){
					next_non_space = next;
					out_str += " ";
					if(next !== ""){
						var j = i + 1;
						while(j < latex.length && latex[j] == " ") ++j;
						next_non_space = latex[j];
						if(!next_non_space || next_non_space === " "){
							next_non_space = "";
						}
					}
					if(operators.indexOf(next_non_space) === -1 && !["{", "(", ")", "}", " ", ""].includes(next_non_space)){
						out_str += " * ";
					}
				}
				continue;
			}
			var adding_slash = (on == "\}" && next == "\{");
			if(on == "\{"){
				on = "(";
			} else if(on == "\}"){
				on = ")";
			}
			out_str += on;
			if(on == ")" && (isLetter(next_non_space) || isNumber(next_non_space) || next_non_space === "\\")){
				out_str += " * ";
			}
			if(adding_slash){
				out_str += " / ";
			}
			if(operators.indexOf(next) !== -1){
				out_str += " ";
			}
			if(operators.indexOf(on) !== -1){
				out_str += " ";
			}
			if((isLetter(on) || isNumber(on)) &&
				operators.indexOf(next_non_space) === -1 &&
				!["{", "(", ")", "}", " ", ""].includes(next_non_space)
				&& (isLetter(on) || (isNumber(on) && !isNumber(next_non_space)))){
				out_str += " * ";
			}
			if((isLetter(on) || isNumber(on)) &&
				operators.indexOf(next_non_space) === -1 &&
				next_non_space === "("){
				out_str += " * ";
			}
			if(on === "-" && (prev === "" || prev === "\{" || prev === "(")){
				out_str = out_str.slice(0, -1);
				out_str += "1 * ";
			}
			if(latex[i] !== " "){
				prev = latex[i];
			}
		}
		latex = out_str;

		// Return result
		return latex;
	};
	var scheme2latex = function(output_raw){
		var output = '';
		if(output_raw[0] == "(" && output_raw[output_raw.length - 1] == ")"){
			output_raw = output_raw.slice(1, -1);
		}
		var passed_exp = false;
		output_raw = output_raw.replaceAll("-1 '* ", "-");
		for(var i = 0; i < output_raw.length; i++){
			var on = output_raw[i];
			var next = "";
			var j = i + 1;
			while(j < output_raw.length && output_raw[j] === " ") ++j;
			if(output_raw[j] !== " ") next = output_raw[j];
			if(on == "'") continue;
			if(on == "*"){
				// output += "\\ ";
				if(next !== "("){
					continue;
				}
			}
			if(on == "^"){
				passed_exp = true;
			}
			if(on == "("){
				on = "\\left(";
				if(passed_exp){
					passed_exp = false;
					on = "\{" + on;
					var j = i;
					var counter = 1;
					while(++j < output_raw.length){
						var at = output_raw[j];
						if(!counter) break;
						if(at == "(") ++counter;
						else if(at == ")") --counter;
					}
					output_raw = output_raw.slice(0, j) + "\}" + output_raw.slice(j);
				}
			} else if(on ==")"){
				on = "\\right)";
			}
			if(on != " " && on != "^"){
				passed_exp = false;
			}
			output += on;
		}
		for(var func of IMPLEMENTED_FUNCTIONS.split(" ")){
			var regex = new RegExp("(( )|^)(" + func + ")( )", "gm");
			var replacement = "\\" + func;
			output = output.replace(regex, replacement);
		}
		return output;
	};

	// Initialize output box
	// var problemSpan = document.getElementById('problem');
	// MQ.StaticMath(problemSpan);
	// Define output helper functions.
	var tock = 1, tockers = ["#problem", "#problem2"];
	var generateSchemeCmd = function(ascii_rep, wrt_arr) {
		var ret = `'(${ascii_rep})`;
		var had_one = false;
		var fn_var = '';
		if(ret.includes(" = ")){
			fn_var = `'${fnWrtDisplaySpan.latex()}`;
		} else {
			fn_var = '\'()';
		}
		while(wrt_arr.length > 0){
			if(had_one){
				ret = `(car ${ret})`;
			}
			ret = `(${unmangledFnName} ${ret} '${wrt_arr.shift()} ${fn_var})`;
			had_one = true;
		}
		return ret;
	};
	var lastFuncString = "f(x)";
	var lastLatexEqn = "x";
	var lastDetectedVarNum = 1;
	var handleOutputChange = function(wrt) {
		// Wait for environment to load
		if(!envLoaded){
			return setTimeout(function() {
				handleOutputChange(wrt);
			}, 300);
		}

		// Modify DOM element
		if(ignoreColors > 0){
			if(--ignoreColors >= 1){
				return;
			}
		}
		hideError();
		prepCodeArea();

		// Generate w.r.t. preamble and postamble
		var wrtVar = wrt || wrtMathField.latex();
		var wrtArr = wrtVar
			.split(",")
			.filter(x => x.length > 0)
			.map(x => x.replaceAll("\\ ", ""))
		;
		if(wrt){
			lastLatexEqn = answerMathField.latex(); // can't handle higher-order derivatives of implicitly defined relations yett
			wrtDisplaySpan.latex(`f_{${wrtArr.join("")}}`);
		}
		wrtArr = wrtArr.map(x => x.replaceAll('\\', ''));
		var wrtNum = wrtArr.length;
		(function() {
			var wrtArrTemp = wrtArr.map(x => `\\partial ${x}`);
			var stack = [];
			for(var item of wrtArrTemp){
				if(stack.length > 0){
					var top = stack[stack.length - 1];
					if(item == top[0]){
						stack.push([stack.pop()[0], top[1] + 1]);
						continue;
					}
				}
				stack.push([item, 1]);
			}
			stack = stack.map((x) => {
				if(x[1] == 1) return x[0];
				return `${x[0]}^{${x[1]}}`;
			});
			stack = stack.reverse(); // right-most partial is inside-most evaluation
			wrtVar = stack.join(" ");
		})(); // avoid polluting function namespace

		// Parse and process LaTeX input
		var latex_eqn = lastLatexEqn;
		latex_eqn = stripLatexSpaces(latex_eqn);
		var ascii_rep = latex2ascii(latex_eqn);
		console.log("LaTeX:", latex_eqn);
		console.log("ASCII:", ascii_rep);

		// Perform Scheme evaluation
		var scheme_cmd = generateSchemeCmd(ascii_rep, wrtArr);
		var is_implicit = ascii_rep.includes(" = ");
		biwa.evaluate(scheme_cmd, function(result) {
			// Update DOM accordingly
		  	showSuccessColors();
		  	var output_res = result.to_array();
	  	  	if(wrtNum == 1){
	  			var detected_vars = output_res[1];
	  			if(detected_vars.to_set){
	  				detected_vars = detected_vars.to_set().arr;
	  				detected_vars = detected_vars.map(x => x.name).filter(x => !CONSTANT_VAR_NAMES.includes(x));
	  				lastDetectedVarNum = detected_vars.length;
	  				lastFuncString = `f(${detected_vars.join(",\\ ")})`;
	  			}
	  		}
	  		if(is_implicit){
	  			detectedVarsSpan.latex(`${latex_eqn}`);
	  			lastFuncString = fnWrtDisplaySpan.latex();
	  		} else {
	  			detectedVarsSpan.latex(`${lastFuncString} = ${latex_eqn}`);
	  		}

		  	// Javascript-ize Scheme output
		  	var output_raw = output_res[0].toString();
		  	console.log("Raw output:", output_raw);
		  	var output = scheme2latex(output_raw);
		  	console.log("Processed:", output);

		  	// Generate LaTeX display
		  	latex_eqn = latex_eqn.replaceAll("operatorname", "text");
		  	var preamble = `{:(\\frac{\\partial${(wrtNum > 1 ? `^{${wrtNum}}` : "")}}{${wrtVar}}\\left(`;
		  	if(lastDetectedVarNum == 1){
		  		preamble = preamble.replaceAll("\\partial", "d");
		  	}
		  	var postamble = `\\right) =,${output}):}`;
		  	var latex_str = preamble + lastFuncString.replaceAll('\\ ', '') + postamble;
		  	var tock_id = tockers[tock];
		  	$(tock_id).css('display', 'none');
		  	$(tock_id).html("`" + latex_str + "`");

		  	// Queue typesetting changes
		  	MathJax.Hub.Queue(["Typeset", MathJax.Hub, tock_id]);
		  	var next_tock = 1 - tock;
		  	MathJax.Hub.Queue(function() {
		  		// console.log("Called");
		  		$(tockers[next_tock]).css('display', 'none');
		  		$(tock_id).css('display', 'inherit');
		  	});
		  	refreshCode();
		  	tock = next_tock;
		});
	};

	// Initialize input box for expressions
	var answerSpan = document.getElementById('answer');
	var exprHandler = function() {
		lastBoxTyping = ['answer'];
		lastLatexEqn = answerMathField.latex(); // Get entered math in LaTeX format
		setTimeout(handleOutputChange, 50);
	};
	var relationHandler = function() {
		lastBoxTyping = ['answer_lhs', 'answer_rhs'];
		lastLatexEqn = answerLeftField.latex() + "=" + answerRightField.latex(); // Get entered math in LaTeX format
		setTimeout(handleOutputChange, 50);
	};
	var answerMathField = MQ.MathField(answerSpan, {
		autoOperatorNames: IMPLEMENTED_FUNCTIONS,
		autoCommands: GREEK_LETTERS + ' sqrt',
		handlers: {
			edit: exprHandler
		}
	});

	// Initialize input boxes for relations
	var answerLeftField = MQ.MathField(document.getElementById('answer_lhs'), {
		autoOperatorNames: IMPLEMENTED_FUNCTIONS,
		autoCommands: GREEK_LETTERS + ' sqrt',
		handlers: {
			edit: relationHandler
		}
	});
	var answerRightField = MQ.MathField(document.getElementById('answer_rhs'), {
		autoOperatorNames: IMPLEMENTED_FUNCTIONS,
		autoCommands: GREEK_LETTERS + ' sqrt',
		handlers: {
			edit: relationHandler
		}
	});

	// Initialize input box for w.r.t. variable
	var wrtSpan = document.getElementById('wrt');
	var wrtMathField = MQ.MathField(wrtSpan, {
		autoCommands: GREEK_LETTERS,
		handlers: {
			edit: function() {
				lastBoxTyping = ['answer'];
				setTimeout(handleOutputChange, 50);
			}
		}
	});

	// Handle arbitrarily deep / mixed derivatives
	var wrtArrMathField = MQ.MathField(document.getElementById('wrt_arr'), {
		autoCommands: GREEK_LETTERS,
		handlers: {
			edit: function() {
				setTimeout(() => {
					lastBoxTyping = ['answer'];
					handleOutputChange(wrtArrMathField.latex());
				}, 50);
			}
		}
	});
	var orderNumMathField = MQ.MathField(document.getElementById('order_num'), {
		handlers: {
			edit: function() {
				lastBoxTyping = ['answer'];
				setTimeout(handleOutputChange, 50);
			}
		}
	});
	var fnWrtDisplaySpan = MQ.MathField(document.getElementById('answer_fn'), {
		autoCommands: GREEK_LETTERS,
		handlers: {
			edit: relationHandler
		}
	});

	// Initialize output fields
	const SAMPLE_EXPRESSIONS = [
		"\\sqrt{x^3+y^3}",
		"\\sin (x^2+y^2)+y^x+\\sqrt{x^2}",
		"e^{xyz^2}",
		"x^{\\frac{2}{xyz}+e^x}",
		"x^x\\ln \\left(e^{ex}\\right)",
		"x^{3xy}"
	];
	var getRandomElement = function(items) {
		return items[Math.floor(Math.random()*items.length)];
	};
	var detectedVarsSpan = MQ.MathField(document.getElementById('detected_vars'), {});
	var wrtDisplaySpan = MQ.MathField(document.getElementById('wrt_disp'), {});
	setTimeout(() => {
		ignoreColors = 2; // one extra edit() handler call is generated by latex() below, so we want to save work
		answerMathField.latex(getRandomElement(SAMPLE_EXPRESSIONS));
	}, 100);

	// Initialize user-controlled buttons
	var isModalHiding = false;
	$('#userModal').on('hide.bs.modal', function() {
		isModalHiding = true;
	});
	$('#userModal').on('hidden.bs.modal', function() {
		isModalHiding = false;
	});
	var dynamicModalResize = function() {
		if(isModalHiding) return;
		$('#userModal').find('.modal-dialog').css({
			"min-width": "200px",
			"max-width": `${$('#userModal').find("table.table").width() + 16 * 2 + 2}px`
		});
	};
	var displayModal = function() {
		jQuery.noConflict();
		$('#userModal').find('.modal-dialog').css({
			"max-width": "500px"
		});
		$('#userModal').modal('show');
	};
	$('#about_btn').click(function() {
		$('#userModal').find(".modal-title").text("About This Project");
		var html_content = `Developed by <a href="mailto:sumer.kohli@berkeley.edu">Sumer Kohli</a> at UC Berkeley. <br style="line-height: 1.4em;" /> Inspired by CS61A Lab 9. <br style="line-height: 1.4em;" /> Tested by <a href="mailto:neelesh.r@berkeley.edu">Neelesh R.</a>.`;
		$('#userModal').find(".modal-body").html(html_content);
		displayModal();
		return false;
	});
	$('#supported_fn_btn').click(function() {
		$('#userModal').find(".modal-title").text("Supported Functions");
		var html_content = $('<table class="table table-striped table-hover"><thead><tr><th scope="col">Description</th><th scope="col">Text</th><th scope="col">Formatted</th></tr></thead></table>');
		var tbody = $('<tbody></tbody>');
		var desc = {
			"sin": "Sine",
			"cos": "Cosine",
			"tan": "Tangent",
			"sec": "Secant",
			"csc": "Cosecant",
			"cot": "Cotangent",
			"sinh": "Hyperbolic Sine",
			"cosh": "Hyperbolic Cosine",
			"tanh": "Hyperbolic Tangent",
			"sech": "Hyperbolic Secant",
			"csch": "Hyperbolic Cosecant",
			"coth": "Hyperbolic Cotangent",
			"log": "Natural Log",
			"ln": "Natural Log",
			"sqrt": "Square Root",
			// "abs": "Absolute Value",
			"arcsin": "Inverse Sine",
			"arccos": "Inverse Cosine",
			"arctan": "Inverse Tangent",
			"arcsec": "Inverse Secant",
			"arccsc": "Inverse Cosecant",
			"arccot": "Inverse Cotangent",
		};
		for(var func of IMPLEMENTED_FUNCTIONS.split(" ")){
			var row = $('<tr></tr>');
			if(!(func in desc)){
				continue;
			}
			$(`<th scope="row">${desc[func]}</th>`).appendTo(row);
			$(`<th>${func}</th>`).appendTo(row);
			$(`<th>\`${func}(x)\`</th>`).appendTo(row);
			row.appendTo(tbody);
		}
		tbody.appendTo(html_content);
		$('#userModal').find(".modal-body").html(html_content);
		MathJax.Hub.Queue(["Typeset", MathJax.Hub, '#userModal']);
		MathJax.Hub.Queue(dynamicModalResize);
		displayModal();
		return false;
	});
	$('#sample_btn').click(function() {
		$('#userModal').find(".modal-title").text("Sample Expressions");
		var html_content = $('<table class="table table-striped table-hover"><thead><tr><th scope="col">Text</th><th scope="col">Formatted</th></tr></thead></table>');
		var tbody = $('<tbody></tbody>');
		for(var func of SAMPLE_EXPRESSIONS){
			var row = $('<tr></tr>');
			$(`<th scope="row">${func}</th>`).appendTo(row);
			$(`<th>\`${func}\`</th>`).appendTo(row);
			row.appendTo(tbody);
		}
		tbody.appendTo(html_content);
		$('#userModal').find(".modal-body").html(html_content);
		MathJax.Hub.Queue(["Typeset", MathJax.Hub, '#userModal']);
		MathJax.Hub.Queue(dynamicModalResize);
		displayModal();
		return false;
	});

	// Initialize top-right images
	$('.top-right-image').each(function() {
		$(this).css('background-image', `url("${$(this).data("img")}")`);
	});
	MathJax.Hub.Queue(function() {
		$('.top-right-icon').each(function(index, el) {
			setTimeout(function() {
				$(el).addClass('top-right-fade');
				setTimeout(function() {
					$(el).addClass('top-right-transition');
				}, 300);
			}, (index * 350));
		});
		// $('.top-right-icon').fadeIn(500);
	});
});
