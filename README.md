# Advanced Cloud Logging #

In the Wolfram Cloud, the only types of objects that write to a log file are `ScheduledTasks`. However, even these logs are poorly formatted and difficult to read. This package attempts to create a set of functions that can be utilized in order to provide a better logging system that:

1. Can be wrapped around any expression evaluated in the cloud
2. Is written in a format that is both easy to read and easy to compute with

## Logging ##

To use the Advanced Cloud Logging system for writing to a log file, all you need to do is use the `CloudLoggingWrapper` function. `CloudLoggingWrapper` has two forms, shown below:

```
CloudLoggingWrapper[expr, log]
```

```
CloudLoggingWrapper[expr]
```

As you can see, the first takes both the expression to be evaluated (_expr_) and the location to log to (_log_). This allows you to choose where you would like to save your log as well as use the same log for multiple evaluations. The second form only takes the expression. In this case, an unnamed object will be created to be used for the log file. This use is fine if you only need to log the odd evaluation and are willing to do a little digging to find where the file is.

## Importing the Log ##

One of the nice features of the Advanced Cloud Logging system is that the log files are both easy to read and easy to import. To import a log written using the `CloudLoggingWrapper`, simply use the `ImportCloudLog` function.

## Additional Tools ##

### MessageHistogram ###

`MessageHistogram` shows a `DateHistogram` of the messages in the log file, grouped by their channel (or category). Remember, "Info" messages are from the system, "Output" are from Print statements, and "Messages" and "Urgent" are errors. `MessageHistogram` can either be given a `Dataset` returned by `ImportCloudLog` or a log file created by `CloudLoggingWrapper`.
