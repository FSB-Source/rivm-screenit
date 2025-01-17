package nl.rivm.screenit.clientportaal.filter;

/*-
 * ========================LICENSE_START=================================
 * screenit-clientportaal
 * %%
 * Copyright (C) 2012 - 2025 Facilitaire Samenwerking Bevolkingsonderzoek
 * %%
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 * =========================LICENSE_END==================================
 */

import java.io.IOException;
import java.io.Writer;
import java.util.Scanner;
import java.util.concurrent.atomic.AtomicBoolean;

import org.apache.catalina.connector.Request;
import org.apache.catalina.connector.Response;
import org.apache.catalina.util.ServerInfo;
import org.apache.catalina.valves.ErrorReportValve;
import org.apache.coyote.ActionCode;
import org.apache.tomcat.util.ExceptionUtils;
import org.apache.tomcat.util.res.StringManager;
import org.apache.tomcat.util.security.Escape;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class CustomTomcatErrorValve extends ErrorReportValve
{

	private static final Logger LOGGER = LoggerFactory.getLogger(CustomTomcatErrorValve.class);

	public CustomTomcatErrorValve()
	{
		setShowServerInfo(false);
		setShowReport(false);
	}

	@Override
	protected void report(Request request, Response response, Throwable throwable)
	{
		int statusCode = response.getStatus();
		if (statusCode >= 400 && response.getContentWritten() <= 0L && response.setErrorReported())
		{
			AtomicBoolean result = new AtomicBoolean(false);
			response.getCoyoteResponse().action(ActionCode.IS_IO_ALLOWED, result);
			if (result.get())
			{
				String message = Escape.htmlElementContent(response.getMessage());
				String reason;
				if (message == null)
				{
					if (throwable != null)
					{
						reason = throwable.getMessage();
						if (reason != null && reason.length() > 0)
						{
							message = Escape.htmlElementContent((new Scanner(reason)).nextLine());
						}
					}

					if (message == null)
					{
						message = "";
					}
				}

				reason = null;
				String description = null;
				StringManager smClient = StringManager.getManager("org.apache.catalina.valves", request.getLocales());
				response.setLocale(smClient.getLocale());

				try
				{
					reason = smClient.getString("http." + statusCode + ".reason");
					description = smClient.getString("http." + statusCode + ".desc");
				}
				catch (Throwable t)
				{
					ExceptionUtils.handleThrowable(t);
				}

				if (reason == null || description == null)
				{
					if (message.isEmpty())
					{
						return;
					}

					reason = smClient.getString("errorReportValve.unknownReason");
					description = smClient.getString("errorReportValve.noDescription");
				}

				StringBuilder sb = new StringBuilder();
				sb.append("<!doctype html><html lang=\"");
				sb.append(smClient.getLocale().getLanguage()).append("\">");
				sb.append("<head>");
				sb.append("<title>");
				sb.append(smClient.getString("errorReportValve.statusHeader", new Object[] { String.valueOf(statusCode), reason }));
				sb.append("</title>");
				sb.append("</head><body>");
				sb.append("<h1>");
				sb.append(smClient.getString("errorReportValve.statusHeader", new Object[] { String.valueOf(statusCode), reason })).append("</h1>");
				if (this.isShowReport())
				{
					sb.append("<hr class=\"line\" />");
					sb.append("<p><b>");
					sb.append(smClient.getString("errorReportValve.type"));
					sb.append("</b> ");
					if (throwable != null)
					{
						sb.append(smClient.getString("errorReportValve.exceptionReport"));
					}
					else
					{
						sb.append(smClient.getString("errorReportValve.statusReport"));
					}

					sb.append("</p>");
					if (!message.isEmpty())
					{
						sb.append("<p><b>");
						sb.append(smClient.getString("errorReportValve.message"));
						sb.append("</b> ");
						sb.append(message).append("</p>");
					}

					sb.append("<p><b>");
					sb.append(smClient.getString("errorReportValve.description"));
					sb.append("</b> ");
					sb.append(description);
					sb.append("</p>");
					if (throwable != null)
					{
						String stackTrace = this.getPartialServletStackTrace(throwable);
						sb.append("<p><b>");
						sb.append(smClient.getString("errorReportValve.exception"));
						sb.append("</b></p><pre>");
						sb.append(Escape.htmlElementContent(stackTrace));
						sb.append("</pre>");
						int loops = 0;

						for (Throwable rootCause = throwable.getCause(); rootCause != null && loops < 10; ++loops)
						{
							stackTrace = this.getPartialServletStackTrace(rootCause);
							sb.append("<p><b>");
							sb.append(smClient.getString("errorReportValve.rootCause"));
							sb.append("</b></p><pre>");
							sb.append(Escape.htmlElementContent(stackTrace));
							sb.append("</pre>");
							rootCause = rootCause.getCause();
						}

						sb.append("<p><b>");
						sb.append(smClient.getString("errorReportValve.note"));
						sb.append("</b> ");
						sb.append(smClient.getString("errorReportValve.rootCauseInLogs"));
						sb.append("</p>");
					}

					sb.append("<hr class=\"line\" />");
				}

				if (this.isShowServerInfo())
				{
					sb.append("<h3>").append(ServerInfo.getServerInfo()).append("</h3>");
				}

				sb.append("</body></html>");

				try
				{
					try
					{
						response.setContentType("text/html");
						response.setCharacterEncoding("utf-8");
					}
					catch (Throwable t)
					{
						ExceptionUtils.handleThrowable(t);
						if (this.container.getLogger().isDebugEnabled())
						{
							this.container.getLogger().debug("status.setContentType", t);
						}
					}

					response.setHeader("Content-Security-Policy", "default-src 'self';");
					response.setHeader("X-Frame-Options", "sameorigin");
					response.setHeader("X-XSS-Protection", "1; mode=block");
					response.setHeader("X-Content-Type-Options", "nosniff");
					response.setHeader("Referrer-Policy", "same-origin");

					Writer writer = response.getReporter();
					if (writer != null)
					{
						writer.write(sb.toString());
						response.finishResponse();
					}
				}
				catch (IOException | IllegalStateException e)
				{
				}
			}
		}
	}
}
