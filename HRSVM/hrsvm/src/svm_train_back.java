import libsvm.*;
import java.io.*;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.*;
import java.util.function.DoublePredicate;
import java.math.BigDecimal;
import java.math.RoundingMode;

import org.rosuda.JRI.Rengine;

public class svm_train_back {
	private svm_parameter param;		// set by parse_command_line
	private svm_problem prob;		// set by read_problem
	private svm_model model;
	private String input_file_name;		// set by parse_command_line
	private String model_file_name;		// set by parse_command_line
	private String error_msg;
	private int cross_validation;
	private int nr_fold;
	
	//DBP2: New parameters
	private int num_points_match;
	private boolean[] source_points_back;
	private int[] source_points_transferred;
	private boolean[] transfer_points_back;
	private String transfer_model_back;
	private svm_problem transfer_prob;
	
	double[] initial_alpha;
	double[] initial_gradients;
	boolean[] misclassified;
	String pair_points_transferred;
	private svm_model[] invariance_models;
	private svm_model[] invariance_models_negative;
	//DBP2 gamma parameter
	private double gamma_invariant;
	
	Random random_generator;
	
	public static Rengine engine;
	
	private int num_points_match_negative;
	private boolean[] source_points_back_negative;
	private boolean[] transfer_points_back_negative;
	String pair_points_transferred_negative;
	private double balance_factor;
	private double previous_c; //based on previous C = 1 / r (rho in nu-SVM paper)
	private double previous_rho;
	private double w_norm; //w_norm for v calculation
	private String previous_performance_file;
	private double previous_performance; //previous performance as a lower bound to calculate v
	private double v; //v parameter of nu-SVM
	private double[] v_grid;
	private String validation_file;
	private String log_file_name;
	private boolean first_nu;
	private String transfer_log_name;
	//DBP2

	private static svm_print_interface svm_print_null = new svm_print_interface()
	{
		public void print(String s) {}
	};

	private static void exit_with_help()
	{
		System.out.print(
		 "Usage: svm_train [options] training_set_file [model_file]\n"
		+"options:\n"
		+"-s svm_type : set type of SVM (default 0)\n"
		+"	0 -- C-SVC		(multi-class classification)\n"
		+"	1 -- nu-SVC		(multi-class classification)\n"
		+"	2 -- one-class SVM\n"
		+"	3 -- epsilon-SVR	(regression)\n"
		+"	4 -- nu-SVR		(regression)\n"
		+"-t kernel_type : set type of kernel function (default 2)\n"
		+"	0 -- linear: u'*v\n"
		+"	1 -- polynomial: (gamma*u'*v + coef0)^degree\n"
		+"	2 -- radial basis function: exp(-gamma*|u-v|^2)\n"
		+"	3 -- sigmoid: tanh(gamma*u'*v + coef0)\n"
		+"	4 -- precomputed kernel (kernel values in training_set_file)\n"
		+"-d degree : set degree in kernel function (default 3)\n"
		+"-g gamma : set gamma in kernel function (default 1/num_features)\n"
		+"-r coef0 : set coef0 in kernel function (default 0)\n"
		+"-c cost : set the parameter C of C-SVC, epsilon-SVR, and nu-SVR (default 1)\n"
		+"-n nu : set the parameter nu of nu-SVC, one-class SVM, and nu-SVR (default 0.5)\n"
		+"-p epsilon : set the epsilon in loss function of epsilon-SVR (default 0.1)\n"
		+"-m cachesize : set cache memory size in MB (default 100)\n"
		+"-e epsilon : set tolerance of termination criterion (default 0.001)\n"
		+"-h shrinking : whether to use the shrinking heuristics, 0 or 1 (default 1)\n"
		+"-b probability_estimates : whether to train a SVC or SVR model for probability estimates, 0 or 1 (default 0)\n"
		+"-wi weight : set the parameter C of class i to weight*C, for C-SVC (default 1)\n"
		+"-v n : n-fold cross validation mode\n"
		+"-q : quiet mode (no outputs)\n"
		//new parameters
		+"-S : source model to transfer from (can be multiple)"
		+"-H : target points back (allows repetition)\n"
		+"-M : name of the refined model for storage\n"
		+"-G : gamma invariant value"
		+"-L : name of the transfer log file produced by AccGenSVM"
		);
		System.exit(1);
	}

	private void do_cross_validation()
	{
		int i;
		int total_correct = 0;
		double total_error = 0;
		double sumv = 0, sumy = 0, sumvv = 0, sumyy = 0, sumvy = 0;
		double[] target = new double[prob.l];

		svm_back.svm_cross_validation(prob,param,nr_fold,target, initial_gradients, initial_alpha, invariance_models, gamma_invariant, source_points_back, log_file_name);
		if(param.svm_type == svm_parameter.EPSILON_SVR ||
		   param.svm_type == svm_parameter.NU_SVR)
		{
			for(i=0;i<prob.l;i++)
			{
				double y = prob.y[i];
				double v = target[i];
				total_error += (v-y)*(v-y);
				sumv += v;
				sumy += y;
				sumvv += v*v;
				sumyy += y*y;
				sumvy += v*y;
			}
			System.out.print("Cross Validation Mean squared error = "+total_error/prob.l+"\n");
			System.out.print("Cross Validation Squared correlation coefficient = "+
				((prob.l*sumvy-sumv*sumy)*(prob.l*sumvy-sumv*sumy))/
				((prob.l*sumvv-sumv*sumv)*(prob.l*sumyy-sumy*sumy))+"\n"
				);
		}
		else
		{
			for(i=0;i<prob.l;i++)
				if(target[i] == prob.y[i])
					++total_correct;
			System.out.print("Cross Validation Accuracy = "+100.0*total_correct/prob.l+"%\n");
		}
	}
	
	/**
	 * @param argv
	 * @throws IOException
	 */
	private void run(String argv[]) throws IOException
	{
		parse_command_line(argv);
		read_problem();
		error_msg = svm_back.svm_check_parameter(prob,param);
		
		//DBP2 Read transfer problem
		read_transfer_problem();
		//complete_read_transfer_problem();
		read_transfer_log();
		//deprecated parameters, do not make an effect
		balance_factor = 1;
		log_file_name = "";
				
		//multiple functions
		if(num_points_match > 0)
		{
			invariance_models = compute_pair_wise_functions();
				
			if(invariance_models.length == 0)
			{
				invariance_models = null;
			}
		}
			
		else 
		{
			gamma_invariant = 0;
		}
		
		//negative
		if(num_points_match_negative > 0)
		{
			invariance_models_negative = compute_pair_wise_functions_negative();
			
			if(invariance_models_negative.length == 0)
			{
				invariance_models_negative = null;
			}
		}

		//calculate v
		if(invariance_models != null && param.nu == 0.5)
		{
			param.nu = calculate_v();
			//param.nu = get_v();
			System.out.println("Selected v = " + param.nu);
			//param.nu = 1;
		}
		
		//DBP2
		if(error_msg != null)
		{
			System.err.print("ERROR: "+error_msg+"\n");
			System.exit(1);
		}
	
		if(cross_validation != 0)
		{
			do_cross_validation();
		}
		else
		{
			//only if there is a sense of previous performance and information with which to update
			if(param.nu > 0.0 && param.nu != 0.5 && invariance_models != null)
			{
				//param.svm_type = 0;
				model = svm_back.svm_train(prob,param, null, initial_alpha, invariance_models, gamma_invariant, source_points_back, log_file_name);
				
				//if(previous_rho == 0)
					//previous_rho = 1;
				
				if(model.r >= previous_rho)
				{
					svm_back.svm_save_model(model_file_name,model);

					if(invariance_models != null)
					{
						System.out.println("\n Number of invariant functions learned: " + invariance_models.length);
					}
				}
				
				else
				{
					save_model_copy();
				}
			}
			
			//else save same model
			else
			{
				save_model_copy();				
			}
		}
	}

	public static void main(String argv[]) throws IOException
	{
		svm_train_back t = new svm_train_back();
		t.run(argv);
	}

	private static double atof(String s)
	{
		double d = Double.valueOf(s).doubleValue();
		if (Double.isNaN(d) || Double.isInfinite(d))
		{
			System.err.print("NaN or Infinity in input\n");
			System.exit(1);
		}
		return(d);
	}

	private static int atoi(String s)
	{
		return Integer.parseInt(s);
	}

	private void parse_command_line(String argv[])
	{
		int i;
		svm_print_interface print_func = null;	// default printing to stdout

		param = new svm_parameter();
		// default values
		param.svm_type = svm_parameter.C_SVC;
		param.kernel_type = svm_parameter.RBF;
		param.degree = 3;
		param.gamma = 0;	// 1/num_features
		param.coef0 = 0;
		param.nu = 0.5;
		param.cache_size = 100;
		param.C = 1;
		param.eps = 1e-3;
		param.p = 0.1;
		param.shrinking = 1;
		param.probability = 0;
		param.nr_weight = 0;
		param.weight_label = new int[0];
		param.weight = new double[0];
		cross_validation = 0;

		// parse options
		for(i=0;i<argv.length;i++)
		{
			if(argv[i].charAt(0) != '-') break;
			if(++i>=argv.length)
				exit_with_help();
			switch(argv[i-1].charAt(1))
			{
				case 's':
					param.svm_type = atoi(argv[i]);
					break;
				case 't':
					param.kernel_type = atoi(argv[i]);
					break;
				case 'd':
					param.degree = atoi(argv[i]);
					break;
				case 'g':
					param.gamma = atof(argv[i]);
					break;
				case 'r':
					param.coef0 = atof(argv[i]);
					break;
				case 'n':
					param.nu = atof(argv[i]);
					break;
				case 'm':
					param.cache_size = atof(argv[i]);
					break;
				case 'c':
					param.C = atof(argv[i]);
					break;
				case 'e':
					param.eps = atof(argv[i]);
					break;
				case 'p':
					param.p = atof(argv[i]);
					break;
				case 'h':
					param.shrinking = atoi(argv[i]);
					break;
				case 'b':
					param.probability = atoi(argv[i]);
					break;
				case 'q':
					print_func = svm_print_null;
					i--;
					break;					
				case 'H':
					transfer_model_back = argv[i];
					break;
				case 'S':
					input_file_name = argv[i];
					break;
				case 'M':
					model_file_name = argv[i];
					break;
				case 'L':
					transfer_log_name = argv[i];
					break;
				case 'G':
					gamma_invariant = atof(argv[i]);
					break;
				case 'v':
					cross_validation = 1;
					nr_fold = atoi(argv[i]);
					if(nr_fold < 2)
					{
						System.err.print("n-fold cross validation: n must >= 2\n");
						exit_with_help();
					}
					break;
				case 'w':
					++param.nr_weight;
					{
						int[] old = param.weight_label;
						param.weight_label = new int[param.nr_weight];
						System.arraycopy(old,0,param.weight_label,0,param.nr_weight-1);
					}

					{
						double[] old = param.weight;
						param.weight = new double[param.nr_weight];
						System.arraycopy(old,0,param.weight,0,param.nr_weight-1);
					}

					param.weight_label[param.nr_weight-1] = atoi(argv[i-1].substring(2));
					param.weight[param.nr_weight-1] = atof(argv[i]);
					break;
				default:
					System.err.print("Unknown option: " + argv[i-1] + "\n");
					exit_with_help();
			}
		}

		svm_back.svm_set_print_string_function(print_func);

		// determine filenames
		//if(i>=argv.length)
			//exit_with_help();
	}

	// read in a problem (in svmlight format)

	private void read_problem() throws IOException
	{
		BufferedReader fp = new BufferedReader(new FileReader(input_file_name));
		Vector<Double> vy = new Vector<Double>();
		Vector<svm_node[]> vx = new Vector<svm_node[]>();
		int max_index = 0;
		
		//DBP2
		boolean init_line = false;
		//DBP2

		while(true)
		{
			String line = fp.readLine();
			if(line == null) break;
			
			if(line.equals("SV"))
			{
				init_line = true;
			}
			
			//read previous c
			else if(line.startsWith("c "))
			{
				previous_c = Double.valueOf(line.split(" ")[1]);
			}
			
			//is the current model a v-SVM model?
			else if(line.startsWith("svm_type nu_svc"))
			{
				first_nu = true;
			}
			
			else if(line.startsWith("vsvm_rho"))
			{
				String[] st = line.split(" "); 
				
				previous_rho = atof(st[1]);
				int a = 0;
			}
			
			else if(init_line)
			{
				StringTokenizer st = new StringTokenizer(line," \t\n\r\f:");
	
				vy.addElement(atof(st.nextToken()));
				int m = st.countTokens()/2;
				svm_node[] x = new svm_node[m];
				for(int j=0;j<m;j++)
				{
					x[j] = new svm_node();
					x[j].index = atoi(st.nextToken());
					x[j].value = atof(st.nextToken());
				}
				if(m>0) max_index = Math.max(max_index, x[m-1].index);
				vx.addElement(x);
			}
			//DBP2
		}

		prob = new svm_problem();
		prob.l = vy.size();
		prob.x = new svm_node[prob.l][];
		for(int i=0;i<prob.l;i++)
			prob.x[i] = vx.elementAt(i);
		//DBP2 Initialize alpha array
		prob.y = new double[prob.l];
		initial_alpha = new double[prob.l];
		for(int i=0;i<prob.l;i++)
		{
			//DBP2: Update to convert to actual class (SVs can be different from 0); but still keep original coefficients
			double y = vy.elementAt(i);
			initial_alpha[i] = y;
			if(y >=0) { prob.y[i] = 1; } else { prob.y[i] = -1; }
		}
		
		if(param.gamma == 0 && max_index > 0)
			param.gamma = 1.0/max_index;

		if(param.kernel_type == svm_parameter.PRECOMPUTED)
			for(int i=0;i<prob.l;i++)
			{
				if (prob.x[i][0].index != 0)
				{
					System.err.print("Wrong kernel matrix: first column must be 0:sample_serial_number\n");
					System.exit(1);
				}
				if ((int)prob.x[i][0].value <= 0 || (int)prob.x[i][0].value > max_index)
				{
					System.err.print("Wrong input format: sample_serial_number out of range\n");
					System.exit(1);
				}
			}
		fp.close();
	}
		
	
	//DBP2 Read transfer file
	private void read_transfer_problem() throws IOException
	{
		BufferedReader fp = new BufferedReader(new FileReader(transfer_model_back));
		Vector<Double> vy = new Vector<Double>();
		Vector<svm_node[]> vx = new Vector<svm_node[]>();
		int max_index = 0;
		
		//DBP2
		boolean init_line = false;
		//DBP2

		while(true)
		{
			//DBP2 From line 12 (index 11), since source support vectors are from this line
			String line = fp.readLine();
			if(line == null) break;
			
			if(line.equals("SV"))
			{
				init_line = true;
			}
			
			else if(init_line)
			{
				StringTokenizer st = new StringTokenizer(line," \t\n\r\f:");
	
				vy.addElement(atof(st.nextToken()));
				int m = st.countTokens()/2;
				svm_node[] x = new svm_node[m];
				for(int j=0;j<m;j++)
				{
					x[j] = new svm_node();
					x[j].index = atoi(st.nextToken());
					x[j].value = atof(st.nextToken());
				}
				if(m>0) max_index = Math.max(max_index, x[m-1].index);
				vx.addElement(x);
			}
		
			//DBP2
		}

		transfer_prob = new svm_problem();
		transfer_prob.l = vy.size();
		transfer_prob.x = new svm_node[transfer_prob.l][];
		for(int i=0;i<transfer_prob.l;i++)
			transfer_prob.x[i] = vx.elementAt(i);
		//DBP2 Initialize alpha array
		transfer_prob.y = new double[transfer_prob.l];
		for(int i=0;i<transfer_prob.l;i++)
		{
			//keep original coefficient
			transfer_prob.y[i] = vy.elementAt(i);
		}
		
		if(param.gamma == 0 && max_index > 0)
			param.gamma = 1.0/max_index;

		if(param.kernel_type == svm_parameter.PRECOMPUTED)
			for(int i=0;i<transfer_prob.l;i++)
			{
				if (transfer_prob.x[i][0].index != 0)
				{
					System.err.print("Wrong kernel matrix: first column must be 0:sample_serial_number\n");
					System.exit(1);
				}
				if ((int)transfer_prob.x[i][0].value <= 0 || (int)transfer_prob.x[i][0].value > max_index)
				{
					System.err.print("Wrong input format: sample_serial_number out of range\n");
					System.exit(1);
				}
			}
		fp.close();
	}	
	
	//FOR THE STRATEGY OF COMPUTING MULTIPLE PAIR-WISE FUNCTIONS
	private svm_model[] compute_pair_wise_functions()
	{
		String[] pair_points = pair_points_transferred.split(",");
		svm_model[] models = new svm_model[(int) (pair_points.length * balance_factor)];
		
		for(int i = 0; i < models.length; i++)
		{
			svm_problem pair_wise_problem = new svm_problem();
			pair_wise_problem.l = 2; 
			pair_wise_problem.x = new svm_node[2][];
			pair_wise_problem.y = new double[2];
			
			//source point
			String[] source_target_points = pair_points[i].split("-");
			pair_wise_problem.x[0] = prob.x[Integer.parseInt(source_target_points[0])];
			pair_wise_problem.y[0] = 1;
			//target point
			pair_wise_problem.x[1] = transfer_prob.x[Integer.parseInt(source_target_points[1])];
			pair_wise_problem.y[1] = 1;
			
			svm_parameter param_pair_wise = new svm_parameter();
			// same values as passed to this function, except for one class SVM
			param_pair_wise.svm_type = svm_parameter.ONE_CLASS;
			param_pair_wise.kernel_type = param.kernel_type;
			param_pair_wise.degree = param.degree;
			param_pair_wise.gamma = param.gamma;
			param_pair_wise.coef0 = param.coef0;
			param_pair_wise.nu = 0.5; 
			param_pair_wise.cache_size = param.cache_size;
			param_pair_wise.C = param.C; 
			param_pair_wise.eps = param.eps;
			param_pair_wise.p = param.p;
			param_pair_wise.shrinking = param.shrinking;
			param_pair_wise.probability = param.probability;
			param_pair_wise.nr_weight = param.nr_weight;
			param_pair_wise.weight_label = param.weight_label;
			param_pair_wise.weight = param.weight;
			
			svm_model pair_wise_model = svm_back.svm_train(pair_wise_problem, param_pair_wise, null, null, null, 0, null, log_file_name);
			models[i] = pair_wise_model;
			//add to the list of models
		}  
		return models;
	}
		
	private svm_model[] compute_pair_wise_functions_negative()
	{
		String[] pair_points = pair_points_transferred_negative.split(",");
		svm_model[] models = new svm_model[(int) (pair_points.length * balance_factor)];
		
		for(int i = 0; i < models.length; i++)
		{
			svm_problem pair_wise_problem = new svm_problem();
			pair_wise_problem.l = 2; 
			pair_wise_problem.x = new svm_node[2][];
			pair_wise_problem.y = new double[2];
			
			//source point
			String[] source_target_points = pair_points[i].split("-");
			pair_wise_problem.x[0] = prob.x[Integer.parseInt(source_target_points[0])];
			pair_wise_problem.y[0] = 1;
			//target point
			pair_wise_problem.x[1] = transfer_prob.x[Integer.parseInt(source_target_points[1])];
			pair_wise_problem.y[1] = 1;
			
			svm_parameter param_pair_wise = new svm_parameter();
			// same values as passed to this function, except for one class SVM
			param_pair_wise.svm_type = svm_parameter.ONE_CLASS;
			param_pair_wise.kernel_type = param.kernel_type;
			param_pair_wise.degree = param.degree;
			param_pair_wise.gamma = param.gamma;
			param_pair_wise.coef0 = param.coef0;
			param_pair_wise.nu = 0.5; 
			param_pair_wise.cache_size = param.cache_size;
			param_pair_wise.C = param.C; 
			param_pair_wise.eps = param.eps;
			param_pair_wise.p = param.p;
			param_pair_wise.shrinking = param.shrinking;
			param_pair_wise.probability = param.probability;
			param_pair_wise.nr_weight = param.nr_weight;
			param_pair_wise.weight_label = param.weight_label;
			param_pair_wise.weight = param.weight;
			
			svm_model pair_wise_model = svm_back.svm_train(pair_wise_problem, param_pair_wise, null, null, null, 0, null, log_file_name);
			models[i] = pair_wise_model;
			//add to the list of models
		}  
		return models;
	}
	
		
	public void read_transfer_log()
	{
		num_points_match = 0;
		source_points_back = new boolean[prob.l];
		transfer_points_back = new boolean[transfer_prob.l];
		pair_points_transferred = "";
		
		num_points_match_negative = 0;
		source_points_back_negative = new boolean[prob.l];
		transfer_points_back_negative = new boolean[transfer_prob.l];
		pair_points_transferred_negative = "";
		//transfer_model_back = "";
		
		String[] file =  input_file_name.split("/");
		String file_name = file[file.length - 2] + "/" + file[file.length - 1];
		
		try {
			
			//by parameter
			BufferedReader transfer_log = new BufferedReader(new FileReader(transfer_log_name));
			
			//local
			//BufferedReader transfer_log = new BufferedReader(new FileReader("src/libsvm/transfer_log_final_hyperplane.txt"));
			
			//remote
			//BufferedReader transfer_log = new BufferedReader(new FileReader("transfer_log_final_hyperplane.txt"));
			
			while(true)
			{
				String line = transfer_log.readLine();
				if(line == null) break;

				String[] st = line.split("\t");
				
				String source_name = st[0];
				
				if(source_name.equals(file_name))
				{
					if(st[1].equals("positive"))
					{
						source_points_back[(int) Double.parseDouble(st[2])] = true;
						transfer_points_back[(int) Double.parseDouble(st[3])] = true;		
					
						pair_points_transferred += (int) Double.parseDouble(st[2]) + "-" + (int) Double.parseDouble(st[3]) + ",";
					}
					
					else
					{
						source_points_back_negative[(int) Double.parseDouble(st[2])] = true;
						transfer_points_back_negative[(int) Double.parseDouble(st[3])] = true;		
					
						pair_points_transferred_negative += (int) Double.parseDouble(st[2]) + "-" + (int) Double.parseDouble(st[3]) + ",";
						
					}
				}
			}
			
			transfer_log.close();
			
			//calculate number of matching positive points
			for(int i = 0; i < source_points_back.length; i++)
			{
				if(source_points_back[i])
				{
					num_points_match++;
				}
			}
			
			for(int i = 0; i < transfer_points_back.length; i++)
			{
				if(transfer_points_back[i])
				{
					num_points_match++;
				}
			}
			
			//calculate number of matching negative points
			for(int i = 0; i < source_points_back_negative.length; i++)
			{
				if(source_points_back_negative[i])
				{
					num_points_match_negative++;
				}
			}
			
			for(int i = 0; i < transfer_points_back_negative.length; i++)
			{
				if(transfer_points_back_negative[i])
				{
					num_points_match_negative++;
				}
			}
			
		} 
		
		catch (Exception e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}
	
	//calculate value of v (min)
	public double calculate_v()
	{
			//v is set to the maximal value
			if(count_negatives() != count_positives())
			{
				v = (((2 * Math.min((double) count_positives(), (double) count_negatives())) / prob.l));
			}
			
			else
			{
				v = 0.99;
			}
			
			//set v to 0.99 when larger
			if(v > 0.99)
			{
				v = 0.99;
			}

		if(!first_nu && invariance_models != null)
		{
			v = v - (((double) num_points_match / prob.l) * gamma_invariant);
		}
		v = (double)Math.floor(v * 100000d) / 100000d;
		System.out.println("v = " + v);
		return v;
	}
	
	public int count_positives()
	{
		int num_positives = 0;
		for(int i = 0; i < prob.l; i++)
		{
			if(prob.y[i] > 0)
			{
				num_positives++;
			}
		}
		return num_positives;
	}
	
	public int count_negatives()
	{
		int num_negatives = 0;
		for(int i = 0; i < prob.l; i++)
		{
			if(prob.y[i] < 0)
			{
				num_negatives++;
			}
		}
		return num_negatives;
	}
	
	public void save_model_copy()
	{
		try
		{
			//save .model file
			BufferedReader fp = new BufferedReader(new FileReader(input_file_name));
			DataOutputStream out = new DataOutputStream(new BufferedOutputStream(new FileOutputStream(model_file_name)));
			
			while(true)
			{
				String line = fp.readLine();
				if(line == null) break;
				
				else
				{
					out.writeBytes(line + "\n");
				}
			}
			
			fp.close();
			out.close();
		}
		
		catch(Exception e)
		{
			
		}
	}
}
