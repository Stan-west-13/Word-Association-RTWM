using CSV;
using DataFrames;
using MixedModels;
using StatsModels;
using CategoricalArrays;
using Effects;
using Statistics;
using LinearAlgebra;
## Fixed effects table save function
outfun = function(m, outfile)
    ct = coeftable(m)
    CSV.write(outfile, DataFrame(ct), header = true)
end

## Dataframe
df = CSV.read("data/Julia_df_meta.csv", DataFrame, missingstring = "NA")
df.participant = categorical(df.participant)
df.condition = categorical(df.condition)
df.context = categorical(df.context)
levels!(df.context, ["peer","child","short","creative"])
levels!(df.condition, ["no_load","load"])
## Models
m_aoa = fit(MixedModel, 
    @formula(aoa ~ condition*context + (condition*context| cue) + (condition | participant)), 
    df)
outfun(m_aoa, "data/m_aoa.csv")

m_wf = fit(MixedModel,
    @formula(Lg10WF ~  condition*context + (condition*context| cue) + (condition | participant)), 
    df)
outfun(m_wf, "data/m_wf.csv")

m_wl = fit(MixedModel, 
    @formula(nchar_lg10 ~ condition*context + (condition*context| cue) + (condition | participant)), 
    df)
outfun(m_wl, "data/m_wl.csv")

## Models short ref
levels!(df.context, ["short","child","peer","creative"])

m_aoa_short = fit(MixedModel, 
    @formula(aoa ~ condition*context + (condition*context| cue) + (condition | participant)), 
    df)
outfun(m_aoa_short, "data/m_aoa_short.csv")

m_wf_short = fit(MixedModel,
    @formula(Lg10WF ~ condition*context + (condition*context| cue) + (condition | participant)), 
    df)
outfun(m_wf_short, "data/m_wf_short.csv")

m_wl_short = fit(MixedModel, 
    @formula(nchar_lg10 ~ condition*context + (condition*context| cue) + (condition | participant)), 
    df)
outfun(m_wl_short, "data/m_wl_short.csv")
## Function for CI and marginal means
marginal_meanConfint = function(model,df, measure, outfile)
# --- Step 1: Extract fixed effects coefficients, design matrix, and covariance matrices 
    X = modelmatrix(model)              # fixed effects design matrix
    β = fixef(model)                    # fixed effects coefficients
    Vβ = vcov(model)                  # variance-covariance matrix of fixed effects

# --- Step 2: Compute marginal predictions
    marginal_preds = X * β              # marginal predictions (ignores random effects)

# --- Step 3: Compute standard errors of marginal predictions
# Standard error for each row: sqrt.(diag(X * Vβ * X'))
    pred_se = sqrt.(diag(X * Vβ * X'))

# --- Step 4: Compute 95% confidence intervals
# Normal approximation: mean ± 1.96 * SE
    lower_ci = marginal_preds .- 1.96 .* pred_se
    upper_ci = marginal_preds .+ 1.96 .* pred_se


# --- Step 5: Create DataFrame of marginal means by group
    marginal_df = DataFrame(condition = dropmissing(df,measure).condition,
                            context = dropmissing(df,measure).context,
                            marginal_mean = marginal_preds,
                            lower_ci = lower_ci,
                            upper_ci = upper_ci)

# --- Step 6: Compute group-level marginal means
    group_means = combine(groupby(marginal_df, [:condition, :context]), :marginal_mean => mean => :mean,
        :lower_ci => mean => :ci_lower,
        :upper_ci => mean => :ci_upper)

# --- Step 7: Save output
    CSV.write(outfile, DataFrame(group_means), header = true)
end

marginal_meanConfint(m_aoa, df,:aoa,"data/marginal_means_aoa.csv")
marginal_meanConfint(m_wf, df,:Lg10WF,"data/marginal_means_wf.csv")
marginal_meanConfint(m_wl, df,:nchar_lg10,"data/marginal_means_wl.csv")
