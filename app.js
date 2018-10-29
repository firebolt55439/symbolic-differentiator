$(function() {
	// Instantiate interpreter
	var displayError = function(err) {
		$('#error_message').text(err);
		$('#error_box').show();
	};
	var hideError = function() {
		$('#error_box').hide();
		$('#answer').css('border-color', '');
	};
	var showSuccessColors = function() {
		$('#answer').css('border-color', 'green');
		$('#detected_vars').css('border-color', 'green');
	};
	var showFailureColors = function() {
		$('#answer').css('border-color', 'orange');
		$('#detected_vars').css('border-color', 'orange');
	};
	BiwaScheme.define_libfunc("derivative-dne", 1, 1, function(ar){
		console.warn(ar);
		displayError(ar[0]);
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
		latex = latex.replace("^{ }", "");
		latex = latex.replace("\\cdot", " * ");
		latex = latex.replace("\\left", " ");
		latex = latex.replace("\\right", " ");

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
				operators.indexOf(next) === -1 &&
				!["{", "(", ")", "}", " ", ""].includes(next)
				&& (isLetter(on) || (isNumber(on) && !isNumber(next)))){
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

	// Initialize output box
	// var problemSpan = document.getElementById('problem');
	// MQ.StaticMath(problemSpan);
	// Define output helper functions.
	var tock = 1, tockers = ["#problem", "#problem2"];
	var generateSchemeCmd = function(ascii_rep, wrt_arr) {
		var ret = `'(${ascii_rep})`;
		while(wrt_arr.length > 0){
			ret = `(derive-infix ${ret} '${wrt_arr.shift()})`;
		}
		return ret;
	};
	const GREEK_LETTERS = 'pi theta alpha beta gamma';
	const IMPLEMENTED_FUNCTIONS = 'sin cos tan sinh cosh tanh log ln sqrt';
	const CONSTANT_VAR_NAMES = ["i", "e"] + GREEK_LETTERS.split(' ');
	var handleOutputChange = function(wrt) {
		// Modify DOM element
		hideError();
		var wrtVar = wrt || wrtMathField.latex();
		var wrtArr = wrtVar.split(",");
		wrtVar = "";
		for(var i = wrtArr.length - 1; i >= 0; i--){
			wrtVar += `\\partial ${wrtArr[i]} `;
		}
		var latex_eqn = answerMathField.latex();
		latex_eqn = stripLatexSpaces(latex_eqn);
		var ascii_rep = latex2ascii(latex_eqn);
		console.log("LaTeX:", latex_eqn);
		console.log("ASCII:", ascii_rep);
		var scheme_cmd = generateSchemeCmd(ascii_rep, wrtArr);
		biwa.evaluate(scheme_cmd, function(result) {
			// Javascript-ize Scheme output
		  	showSuccessColors();
		  	var output_res = result.to_array();
		  	var detected_vars = output_res[1].to_set().arr.map(x => x.name).filter(x => !CONSTANT_VAR_NAMES.includes(x));
		  	detectedVarsSpan.latex(detected_vars.join(","));
		  	var output_raw = output_res[0].toString();
		  	console.log("Raw output:", output_raw);
		  	var output = '';
		  	if(output_raw[0] == "(" && output_raw[output_raw.length - 1] == ")"){
		  		output_raw = output_raw.slice(1, -1);
		  	}
		  	var passed_exp = false;
		  	output_raw = output_raw.replace("-1 '* ", "-");
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
		  	console.log("Processed:", output);

		  	// Generate LaTeX display
		  	latex_eqn = latex_eqn.replace("operatorname", "text");
		  	var preamble = `\\frac{\\partial}{${wrtVar}}\\left(`;
		  	var postamble = `\\right) = ${output}`;
		  	var latex_str = preamble + latex_eqn + postamble;
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
		  	tock = next_tock;
		});
	};

	// Initialize input box for equation
	var answerSpan = document.getElementById('answer');
	var answerMathField = MQ.MathField(answerSpan, {
		autoOperatorNames: IMPLEMENTED_FUNCTIONS,
		autoCommands: GREEK_LETTERS + 'sqrt',
		handlers: {
			edit: function() {
				var enteredMath = answerMathField.latex(); // Get entered math in LaTeX format
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

	// Initialize detected variables output
	var detectedVarsSpan = MQ.MathField(document.getElementById('detected_vars'), {});
	showSuccessColors();
});
