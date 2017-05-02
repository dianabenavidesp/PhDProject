import libsvm.*;
import java.io.*;
import java.util.*;

public class svm_train {
	private svm_parameter param;		// set by parse_command_line
	private svm_problem prob;		// set by read_problem
	private svm_model model;
	private String input_file_name;		// set by parse_command_line
	private String model_file_name;		// set by parse_command_line
	private String error_msg;
	private int cross_validation;
	private int nr_fold;
	
	//DBP
	private int max_sources;
	private int num_features;
	private svm_source_hypothesis hyp[];
	private String source_hypothesis_file_name[];
	private String log_file_name;
	private String selected_features;
	private double kl_threshold;
	//DBP

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
		//DBP
		+"-M : maximum number of source hypotheses\n"
		+"-N : number of features\n"
		+"-F : features selected for FNN (separated by comma)\n"
		+"-K : KL-divergence threshold"
		+"-f : training data file\n"
		+"-H : source hypothesis file name\n"
		+"-y : output (model) file name\n"
		+"-a : output (log) file name\n"
		//DBP
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

		//DBP Modified to accept extra-parameters
		svm.svm_cross_validation(prob,param,nr_fold,target, hyp, log_file_name, selected_features, kl_threshold);
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
	
	public void run(String argv[]) throws IOException
	{

		parse_command_line(argv);
		read_problem();
		
		//DBP: Maximum number of hypotheses, as defined by the parameter.
		hyp = new svm_source_hypothesis[max_sources];
		read_source_hypothesis();
		complete_read_source_hypothesis();
		//DBP
		
		error_msg = svm.svm_check_parameter(prob,param);

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
			//DBP: Modified to include hypotheses list, log file name and pre-selected features for FNN, and KL threshold
			model = svm.svm_train(prob,param, hyp, log_file_name, selected_features, kl_threshold);
			//DBP
			svm.svm_save_model(model_file_name,model);
		}
	}

	public static void main(String argv[]) throws IOException
	{
		long start = System.currentTimeMillis();
		svm_train t = new svm_train();
		t.run(argv);
		long end = System.currentTimeMillis();
		System.out.println(end - start);
		System.exit(0);
		
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
		//DBP
		int source_hypothesis_count = 0;
		//DBP

		// parse options
		for(i=0;i<argv.length;i++)
		{
			//DBP For accepting more than 1 source hypothesis
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
				//DBP
				//Extra-parameters				
				case 'f': 
					input_file_name = argv[i];
					break;
					
				case 'y':
					model_file_name = argv[i];
					break;
				
				case 'H':
					source_hypothesis_file_name[source_hypothesis_count] = argv[i];
					source_hypothesis_count++;
					break;
					
				case 'a':
					log_file_name = argv[i];
					break;
					
				case 'M': 
					max_sources = atoi(argv[i]);
					source_hypothesis_file_name = new String[max_sources];
					break;
					
				case 'N': 
					num_features = atoi(argv[i]);
					break;					

				case 'F': 
					selected_features = argv[i].replace("_", ", ");
					break;	
					
				case 'K': 
					kl_threshold = Double.parseDouble(argv[i]);
					break;	
				//DBP
					
				default:
					System.err.print("Unknown option: " + argv[i-1] + "\n");
					exit_with_help();
			}
		}

		svm.svm_set_print_string_function(print_func);

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

		while(true)
		{
			String line = fp.readLine();
			if(line == null) break;

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
			//DBP Num features, as defined by the N parameter.
			svm_node[] temp = new svm_node[num_features];
			
			for(int j=0;j<num_features;j++)
			{
				temp[j] = new svm_node();
				temp[j].index = j+1;
				temp[j].value = exists_node(x, j+1);
			}
			x = temp;
			//DBP
			
			max_index = num_features;
			//if(m>0) max_index = Math.max(max_index, x[m-1].index);
			vx.addElement(x);
		}

		prob = new svm_problem();
		prob.l = vy.size();
		prob.x = new svm_node[prob.l][];
		for(int i=0;i<prob.l;i++)
			prob.x[i] = vx.elementAt(i);
		prob.y = new double[prob.l];
		for(int i=0;i<prob.l;i++)
			prob.y[i] = vy.elementAt(i);

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
	
	// DBP: Read source hypotheses
	private void read_source_hypothesis() throws IOException
	{
		for(int k=0; k<source_hypothesis_file_name.length;k++)
		{
			if(source_hypothesis_file_name[k] != null)
			{
				BufferedReader fp = new BufferedReader(new FileReader(source_hypothesis_file_name[k]));
				Vector<Double> vy = new Vector<Double>();
				Vector<svm_node[]> vx = new Vector<svm_node[]>();
				int max_index = 0;
				//DBP
				int numLine = 0;
				//DBP
		
				while(true)
				{
					String line = fp.readLine();
					if(line == null) break;
					
					//DBP
					//Init from 10th line, first 9 lines are parameters
					//DBP
		
					if(numLine >= 9)
					{
						StringTokenizer st = new StringTokenizer(line," \t\n\r\f:");
		
						vy.addElement(atof(st.nextToken()));
						int m = st.countTokens()/2;
						svm_node[] x = new svm_node[m];
						for(int j=0;j<m;j++)
						{
							try
							{
								x[j] = new svm_node();
								x[j].index = atoi(st.nextToken());
								x[j].value = atof(st.nextToken());
							}
							catch(Exception e)
							{
								j--;
							}
						}
						if(m>0) max_index = Math.max(max_index, x[m-1].index);
						vx.addElement(x);
					}
					numLine++;
				}
		
				hyp[k] = new svm_source_hypothesis();
				hyp[k].l = vy.size();
				hyp[k].x = new svm_node[hyp[k].l][];
				for(int i=0;i<hyp[k].l;i++)
					hyp[k].x[i] = vx.elementAt(i);
				hyp[k].y = new double[hyp[k].l];
				for(int i=0;i<hyp[k].l;i++)
					hyp[k].y[i] = vy.elementAt(i);
		
				if(param.gamma == 0 && max_index > 0)
					param.gamma = 1.0/max_index;
		
				if(param.kernel_type == svm_parameter.PRECOMPUTED)
					for(int i=0;i<hyp[k].l;i++)
					{
						if (hyp[k].x[i][0].index != 0)
						{
							System.err.print("Wrong kernel matrix: first column must be 0:sample_serial_number\n");
							System.exit(1);
						}
						if ((int)hyp[k].x[i][0].value <= 0 || (int)hyp[k].x[i][0].value > max_index)
						{
							System.err.print("Wrong input format: sample_serial_number out of range\n");
							System.exit(1);
						}
					}
		
				fp.close();
			}
		}
	}
	
	// DBP: Read source hypotheses
	private void complete_read_source_hypothesis()
	{
		svm_source_hypothesis temp;
		
		for(int k=0; k<hyp.length;k++)
		{
			if(hyp[k] != null)
			{
				temp = new svm_source_hypothesis();
				temp.l =hyp[k].l;
				temp.x = new svm_node[temp.l][];
				temp.y = hyp[k].y;
				
				for(int m=0; m<hyp[k].l; m++)
				{
					svm_node[] x = new svm_node[prob.x[0].length];
					
					for(int l=0; l<prob.x[0].length; l++)
					{
						x[l] = new svm_node();
						x[l].index = l+1; 
						x[l].value = exists_node(hyp[k].x[m], l+1);
					}
					
					temp.x[m] = x;
				}
				
				hyp[k] = temp;
			}
		}
	}
	
	// DBP: Read source hypotheses
	private double exists_node(svm_node[] x, int index)
	{
		for(int i=0; i<x.length; i++)
		{
			if(x[i].index == index)
			{
				return x[i].value;
			}
		}
		
		return 0;
	}
}
