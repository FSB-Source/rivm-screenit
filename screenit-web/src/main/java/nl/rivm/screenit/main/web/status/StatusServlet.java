package nl.rivm.screenit.main.web.status;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
 * %%
 * Copyright (C) 2012 - 2022 Facilitaire Samenwerking Bevolkingsonderzoek
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
import java.io.InputStream;
import java.util.Date;
import java.util.Properties;

import javax.servlet.ServletException;
import javax.servlet.ServletOutputStream;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.spring.injection.SpringBeanProvider;

import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.hibernate.SQLQuery;

public class StatusServlet extends HttpServlet
{

	private static final long serialVersionUID = 1L;

	private static final Logger LOG = LoggerFactory.getLogger(StatusServlet.class);

	private static final String ONBEKEND = "Onbekend";

	private static final String CONTENT_TYPE = "text/html";

	private static final String ENCODING = "UTF-8";

	@Override
	protected void doGet(HttpServletRequest httpReq, HttpServletResponse httpResp) throws ServletException, IOException
	{
		LOG.debug("Er is een GET verzoek binnengekomen op de StatusServlet");
		maakResponse(httpResp);
	}

	private void maakResponse(HttpServletResponse httpResp)
	{
		LOG.debug("Bezig met maken van Status Response");

		httpResp.setContentType(CONTENT_TYPE);
		httpResp.setCharacterEncoding(ENCODING);

		String applicatieNaam = ONBEKEND;
		String applicatieEnviroment = ONBEKEND;
		String applicatieInstantie = ONBEKEND;
		String version = ONBEKEND;
		String database = ONBEKEND;

		try
		{
			applicatieNaam = SpringBeanProvider.getInstance().getBean(String.class, "applicatieNaam");
			applicatieEnviroment = SpringBeanProvider.getInstance().getBean(String.class, "applicationEnvironment");
			applicatieInstantie = SpringBeanProvider.getInstance().getBean(String.class, "applicatieInstantie");
			HibernateService hibernateService = SpringBeanProvider.getInstance().getBean(HibernateService.class);
			SQLQuery sqlQuery = hibernateService.getHibernateSession().createSQLQuery("select count(*) from algemeen.pref_prefitem;");
			Object result = sqlQuery.uniqueResult();
			if (result != null)
			{
				database = "OK";
			}
		}
		catch (IllegalStateException ise)
		{
			LOG.error(ise.getMessage(), ise);
		}
		version = readVersie();
		if (StringUtils.isBlank(version))
		{
			version = ONBEKEND;
		}

		StringBuilder response = new StringBuilder();
		response.append("<!DOCTYPE html><html lang=\"nl\">");
		response.append("<head><meta charset=\"UTF-8\"><title>ScreenIT - Status</title></head>");
		response.append("<body><h1>Screenit Status Pagina</h1><ul><li>Applicatie Naam: <strong>");
		response.append(applicatieNaam);
		response.append("</strong></li><li>Applicatie Omgeving: <strong>");
		response.append(applicatieEnviroment);
		response.append("</strong></li><li>Applicatie Instantie: <strong>");
		response.append(applicatieInstantie);
		response.append("</strong></li><li>Applicatie Versie: <strong>");
		response.append(version);
		response.append("</strong></li><li>Database: <strong>");
		response.append(database);
		response.append("</strong></li></ul><p><small>Datum/Tijd: ");
		response.append(new Date());
		response.append("</small></p>");
		response.append("</body></html>");

		ServletOutputStream os;
		try
		{
			os = httpResp.getOutputStream();
			os.write(response.toString().getBytes());
			os.close();
		}
		catch (IOException e)
		{
			LOG.error(e.getMessage(), e);
		}
		LOG.debug("Klaar met Status Response");
	}

	protected String readVersie()
	{
		StringBuilder versieBuilder = new StringBuilder();
		Properties applicationProperties = new Properties();
		try (InputStream resourceAsStream = this.getClass().getResourceAsStream("/application.properties");)
		{
			applicationProperties.load(resourceAsStream);
			String version = applicationProperties.getProperty("application.version");
			versieBuilder.append(version);
			if ("SNAPSHOT".equals(version))
			{
				String buildnumber = applicationProperties.getProperty("application.buildnumber");

				if (!"${BUILD_NUMBER}".equals(buildnumber))
				{
					versieBuilder.append("-").append(buildnumber);
				}
			}
		}
		catch (IOException e)
		{
			LOG.error("Could not load application.properties (for version number)");
		}
		return versieBuilder.toString();
	}
}
