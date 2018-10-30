hljs.initHighlightingOnLoad();

String.prototype.replaceAll = function(search, replacement) {
	// From https://stackoverflow.com/questions/1144783/how-to-replace-all-occurrences-of-a-string-in-javascript
    var target = this;
    return target.split(search).join(replacement);
};

$(function() {
	// Instantiate interpreter
	var displayError = function(err) {
		$('#error_message').text(err);
		$('#error_box').show();
	};
	var hideError = function() {
		$('#error_box').hide();
		$('#answer').removeClass('syntax-outline').removeClass('success-outline');
	};
	var showSuccessColors = function() {
		$('#answer').removeClass('syntax-outline').addClass('success-outline');
	};
	var showFailureColors = function() {
		$('#answer').addClass('syntax-outline').removeClass('success-outline');
	};
	var refreshCode = function() {
		$('#debug_area').show();
		$('code.scheme').each(function(i, block){
			hljs.highlightBlock(block);
		});
	};
	var handleMessage = function(type, data){
		data = data.toString();
		var idx = ["infix-input", "parsed-infix", "simplified-infix", "derivative-prefix", "derivative-infix"];
		if(idx.includes(type)){
			$($('.debug_outputs').get(idx.indexOf(type))).text(data);
		}
	};
	BiwaScheme.define_libfunc("derivative-dne", 1, 1, function(ar){
		console.warn(ar);
		displayError(ar[0]);
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
	biwa.evaluate("(load \"/diff.scm\")", function(result) {
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
		messageStyle: "none"
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
				if(latex[j] == " "){
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
						if(latex[j] == " "){
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
			if(on == ")" && (isLetter(next_non_space) || isNumber(next_non_space))){
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
		while(wrt_arr.length > 0){
			if(had_one){
				ret = `(car ${ret})`;
			}
			ret = `(derive-infix ${ret} '${wrt_arr.shift()})`;
			had_one = true;
		}
		return ret;
	};
	const GREEK_LETTERS = 'pi theta alpha beta gamma';
	const IMPLEMENTED_FUNCTIONS = 'sin cos tan sinh cosh tanh log ln sqrt abs';
	const CONSTANT_VAR_NAMES = ["i", "e"] + GREEK_LETTERS.split(' ');
	var lastFuncString = "f(x)";
	var lastLatexEqn = "x";
	var lastDetectedVarNum = 1;
	var handleOutputChange = function(wrt) {
		// Modify DOM element
		hideError();

		// Generate w.r.t. preamble and postamble
		var wrtVar = wrt || wrtMathField.latex();
		var wrtArr = wrtVar
			.split(",")
			.filter(x => x.length > 0)
			.map(x => x.replaceAll("\\ ", ""))
		;
		if(wrt){
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
		  	detectedVarsSpan.latex(`${lastFuncString} = ${latex_eqn}`);

		  	// Javascript-ize Scheme output
		  	var output_raw = output_res[0].toString();
		  	console.log("Raw output:", output_raw);
		  	var output = scheme2latex(output_raw);
		  	console.log("Processed:", output);

		  	// Generate LaTeX display
		  	latex_eqn = latex_eqn.replaceAll("operatorname", "text");
		  	var preamble = `\\frac{\\partial${(wrtNum > 1 ? `^{${wrtNum}}` : "")}}{${wrtVar}}\\left(`;
		  	if(lastDetectedVarNum == 1){
		  		preamble = preamble.replaceAll("\\partial", "d");
		  	}
		  	var postamble = `\\right) = ${output}`;
		  	var latex_str = preamble + lastFuncString + postamble;
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
	var answerMathField = MQ.MathField(answerSpan, {
		autoOperatorNames: IMPLEMENTED_FUNCTIONS,
		autoCommands: GREEK_LETTERS + ' sqrt',
		handlers: {
			edit: function() {
				lastLatexEqn = answerMathField.latex(); // Get entered math in LaTeX format
				setTimeout(handleOutputChange, 50);
			}
		}
	});

	// Initialize input boxes for relations
	var answerLeftField = MQ.MathField(document.getElementById('answer_lhs'), {
		autoOperatorNames: IMPLEMENTED_FUNCTIONS,
		autoCommands: GREEK_LETTERS + ' sqrt',
		handlers: {
			edit: function() {
				lastLatexEqn = answerLeftField.latex() + "=" + answerRightField.latex(); // Get entered math in LaTeX format
				setTimeout(handleOutputChange, 50);
			}
		}
	});
	var answerRightField = MQ.MathField(document.getElementById('answer_rhs'), {
		autoOperatorNames: IMPLEMENTED_FUNCTIONS,
		autoCommands: GREEK_LETTERS + ' sqrt',
		handlers: {
			edit: function() {
				lastLatexEqn = answerLeftField.latex() + "=" + answerRightField.latex(); // Get entered math in LaTeX format
				setTimeout(handleOutputChange, 50);
			}
		}
	});

	// Initialize input box for w.r.t. variable
	var wrtSpan = document.getElementById('wrt');
	var wrtMathField = MQ.MathField(wrtSpan, {
		autoCommands: GREEK_LETTERS,
		handlers: {
			edit: function() {
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
					handleOutputChange(wrtArrMathField.latex());
				}, 50);
			}
		}
	});
	var orderNumMathField = MQ.MathField(document.getElementById('order_num'), {
		handlers: {
			edit: function() {
				setTimeout(handleOutputChange, 50);
			}
		}
	});

	// Initialize output fields
	var detectedVarsSpan = MQ.MathField(document.getElementById('detected_vars'), {});
	showSuccessColors();
	var wrtDisplaySpan = MQ.MathField(document.getElementById('wrt_disp'), {});
});
