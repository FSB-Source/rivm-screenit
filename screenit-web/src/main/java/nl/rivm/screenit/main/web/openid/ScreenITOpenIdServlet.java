package nl.rivm.screenit.main.web.openid;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
 * %%
 * Copyright (C) 2012 - 2020 Facilitaire Samenwerking Bevolkingsonderzoek
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
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import java.util.UUID;

import javax.servlet.ServletException;
import javax.servlet.ServletOutputStream;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.model.Gebruiker;
import nl.rivm.screenit.model.Instelling;
import nl.rivm.screenit.model.InstellingGebruiker;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.model.enums.ToegangLevel;
import nl.rivm.screenit.service.AutorisatieService;
import nl.rivm.screenit.service.OrganisatieZoekService;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.spring.injection.SpringBeanProvider;

import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.apache.wicket.Session;
import org.openid4java.association.AssociationException;
import org.openid4java.message.AuthRequest;
import org.openid4java.message.AuthSuccess;
import org.openid4java.message.DirectError;
import org.openid4java.message.Message;
import org.openid4java.message.MessageException;
import org.openid4java.message.MessageExtension;
import org.openid4java.message.ParameterList;
import org.openid4java.message.ax.AxMessage;
import org.openid4java.message.ax.FetchRequest;
import org.openid4java.message.ax.FetchResponse;
import org.openid4java.message.sreg.SRegMessage;
import org.openid4java.message.sreg.SRegRequest;
import org.openid4java.message.sreg.SRegResponse;
import org.openid4java.server.ServerException;
import org.openid4java.server.ServerManager;
import org.springframework.http.HttpHeaders;
import org.springframework.util.CollectionUtils;

public class ScreenITOpenIdServlet extends HttpServlet
{

	private static final long serialVersionUID = 1L;

	private static final Logger LOG = LoggerFactory.getLogger(ScreenITOpenIdServlet.class);

	private transient ServerManager serverManager;

	public ScreenITOpenIdServlet()
	{
		if (serverManager == null)
		{
			LOG.debug("serverManager was null");
			this.serverManager = ScreenITOpenIdProviderHelper.getServerManager();
		}
		else
		{
			LOG.debug("serverManager was niet null");
		}
	}

	@Override
	protected void doGet(HttpServletRequest req, HttpServletResponse resp) throws ServletException, IOException
	{
		try
		{
			processRequest(req, resp);
		}
		catch (Exception e)
		{
			LOG.error(e.getMessage(), e);
		}
	}

	@Override
	protected void doPost(HttpServletRequest req, HttpServletResponse resp) throws ServletException, IOException
	{
		try
		{
			processRequest(req, resp);
		}
		catch (Exception e)
		{
			LOG.error(e.getMessage(), e);
		}
	}

	public void processRequest(HttpServletRequest httpReq, HttpServletResponse httpResp) throws Exception
	{
		LOG.debug("----- Verzoek komt binnen -----");
		ParameterList request = new ParameterList(httpReq.getParameterMap());
		String responseText = null;

		String mode = "";
		if (request.hasParameter("openid.mode"))
		{
			mode = request.getParameterValue("openid.mode");
		}

		if (request == null || request != null && CollectionUtils.isEmpty(request.getParameters()) || StringUtils.isBlank(mode))
		{
			LOG.debug("Een verzoek zonder parameters/lege openid.mode, we nemen aan dat dit een Discovery verzoek is.");
			if (request.hasParameter("openid.claimed_id"))
			{
				LOG.debug("Discovery verzoek met Claimed-ID: " + request.getParameterValue("openid.claimed_id"));
			}
			httpResp.setContentType("application/xrds+xml");
			httpResp.setHeader(HttpHeaders.CONTENT_DISPOSITION, "attachment; filename=\"openid\"");
			responseText = ScreenITOpenIdProviderHelper.maakDiscoveryResponse();
			LOG.debug("responseText: " + responseText);
		}
		else
		{
			LOG.info("Verwerk een OpenID verzoek: " + mode);

			if ("associate".equals(mode))
			{
				LOG.debug("Een associate verzoek verwerken");
				Message message = ScreenITOpenIdProviderHelper.verwerkAssociatieRequest(serverManager, request);
				responseText = message.keyValueFormEncoding();
				LOG.debug("responseText: " + responseText);
			}
			else if ("check_authentication".equals(mode))
			{
				LOG.info("Een check authentication verzoek verwerken");
				Message message = ScreenITOpenIdProviderHelper.verwerkVerificatieRequest(serverManager, request);
				responseText = message.keyValueFormEncoding();
				LOG.debug("responseText: " + responseText);

			}
			else if ("checkid_immediate".equals(mode) || "checkid_setup".equals(mode))
			{
				LOG.info("Een check immediate/setup verzoek verwerken");
				if (getSession() != null && getSession().isSignedIn())
				{
					InstellingGebruiker instGeb = getSession().getLoggedInInstellingGebruiker();
					ToegangLevel level = getSession().getToegangsLevel(Actie.INZIEN, Recht.GEBRUIKER_RAPPORTAGE);
					if (level != null)
					{
						LOG.info("Colonis gebruiker is ingelogd en heeft de juiste rechten.");
						String openIdIdentifier = getOpenIdIdentifier(instGeb.getMedewerker().getId(), false);
						String openIdClaimedId = getOpenIdIdentifier(instGeb.getMedewerker().getId(), true);
						Message message = maakAuthResponse(request, openIdIdentifier, openIdClaimedId, maakOpenIdWrapper(instGeb), true);
						if (message instanceof DirectError)
						{
							maakResponse(httpResp, message.keyValueFormEncoding());
						}
						else
						{
							httpResp.setStatus(302);
							httpResp.setHeader(HttpHeaders.LOCATION, message.getDestinationUrl(true));
						}
					}
					LOG.debug("responseText: " + responseText);
				}
				else if ("cancel".equals("mode"))
				{
					LOG.info("OpenID verbinding is geannuleerd");
				}
				else
				{
					LOG.warn("UltimView authenticatie verzoek zonder dat er een ScreenitSessie is. Autheticatie wordt gestopt. Gebruiker wordt geredirect naar Applicatie URL.");
					String url = SpringBeanProvider.getInstance().getBean(String.class, "applicationUrl");
					httpResp.setStatus(302);
					httpResp.setHeader(HttpHeaders.LOCATION, url);
					LOG.debug("responseText: " + responseText);
				}
			}
			else
			{
				LOG.error("Onbekende verzoek mode: '" + mode + "'.");
				Message message = DirectError.createDirectError("Unknown request");
				responseText = message.keyValueFormEncoding();
				LOG.debug("responseText: " + responseText);
			}
		}

		maakResponse(httpResp, responseText);
	}

	private String getOpenIdIdentifier(Long gebruikerId, boolean claimed)
	{
		LOG.trace("Bezig met ophalen van OpenID identifier / claimed Id");
		HibernateService hibernateService = SpringBeanProvider.getInstance().getBean(HibernateService.class);
		StringBuilder identifierSb = new StringBuilder();
		Gebruiker gebruiker = hibernateService.load(Gebruiker.class, gebruikerId);
		if (StringUtils.isBlank(gebruiker.getOpenid()))
		{
			UUID randomId = UUID.randomUUID();
			gebruiker.setOpenid(randomId.toString());
			hibernateService.saveOrUpdate(gebruiker);
		}
		identifierSb.append(ScreenITOpenIdProviderHelper.getOpEndpointUrl());
		LOG.debug("OpenId Identifier: " + identifierSb.toString());
		LOG.trace("Klaar met ophalen van OpenID identifier / claimed Id");
		return identifierSb.toString();
	}

	private void maakResponse(HttpServletResponse httpResp, String response) throws IOException
	{
		httpResp.setCharacterEncoding("UTF-8");
		if (StringUtils.isNotBlank(response))
		{
			ServletOutputStream os = httpResp.getOutputStream();
			os.write(response.getBytes());
			os.close();
		}
		if (getSession() != null)
		{
			getSession().detach();
		}
		LOG.debug("----- Antwoord verstuurd -----");
	}

	private ScreenITOpenIdWrapper maakOpenIdWrapper(InstellingGebruiker instGebruiker)
	{
		HibernateService hibernateService = SpringBeanProvider.getInstance().getBean(HibernateService.class);
		AutorisatieService autorisatieService = SpringBeanProvider.getInstance().getBean(AutorisatieService.class);
		OrganisatieZoekService zoekService = SpringBeanProvider.getInstance().getBean(OrganisatieZoekService.class);
		Gebruiker gebruiker = hibernateService.load(Gebruiker.class, instGebruiker.getMedewerker().getId());
		Instelling instelling = hibernateService.load(Instelling.class, instGebruiker.getOrganisatie().getId());

		ToegangLevel level = autorisatieService.getToegangLevel(instGebruiker, Actie.INZIEN, true, Recht.GEBRUIKER_RAPPORTAGE);

		ScreenITOpenIdWrapper wrapper = new ScreenITOpenIdWrapper();
		wrapper.setEmailadres(gebruiker.getEmailwerk());
		wrapper.setMedewerkerId(gebruiker.getId());
		wrapper.setOrganisatieId(instelling.getId());
		wrapper.setVolledigenaam(gebruiker.getNaamVolledig());
		wrapper.setOrganisatieType(instelling.getOrganisatieType().name());
		if (level != null)
		{
			wrapper.setToegangslevel(level.name());
			wrapper.setZichtbareOrganisaties(zoekService.getZichtbareUltimviewInstellingIds(instelling, level));
		}
		else
		{
			wrapper.setZichtbareOrganisaties(Collections.singletonList(instelling.getId()));
		}
		return wrapper;
	}

	private ScreenitSession getSession()
	{
		if (ScreenitSession.exists())
		{
			LOG.debug("screenitsession");
			return ScreenitSession.get();
		}
		else if (Session.exists())
		{
			LOG.debug("NO screenitsession");
			return (ScreenitSession) Session.get();
		}
		else
		{
			LOG.warn("Screenit Session is null.");
			return null;
		}
	}

	public Message maakAuthResponse(ParameterList requestParameters, String userId, String geclaimedUserId, ScreenITOpenIdWrapper openIdWrapper, boolean geauth)
		throws MessageException, ServerException, AssociationException
	{
		LOG.trace("Start met maken autorisatie reactie");
		LOG.debug("identity: " + userId);
		LOG.debug("claimed id: " + geclaimedUserId);

		Message authResponse = serverManager.authResponse(requestParameters, userId, geclaimedUserId, geauth);

		AuthRequest authRequest = AuthRequest.createAuthRequest(requestParameters, serverManager.getRealmVerifier());

		if (authResponse instanceof DirectError)
		{
			LOG.debug("Autorisatie verzoek is niet ok, verzoek wordt afgebroken.");

		}
		else
		{

			if (authRequest.hasExtension(SRegMessage.OPENID_NS_SREG))
			{
				LOG.debug("Verwerken SR verzoek");
				MessageExtension extensionRequestObject = authRequest.getExtension(SRegMessage.OPENID_NS_SREG);
				if (extensionRequestObject instanceof SRegRequest)
				{
					SRegRequest sRegRequest = (SRegRequest) extensionRequestObject;
					Map<String, Object> registrationData = new HashMap<>();
					registrationData.put("email", openIdWrapper.getEmailadres());
					registrationData.put("fullname", openIdWrapper.getVolledigenaam());
					registrationData.put("medewerkerId", openIdWrapper.getMedewerkerId());
					registrationData.put("organisatieId", openIdWrapper.getOrganisatieId());
					registrationData.put("organisatieType", openIdWrapper.getOrganisatieType());
					SRegResponse sRegResponse = SRegResponse.createSRegResponse(sRegRequest, registrationData);
					authResponse.addExtension(sRegResponse);
				}
				else
				{
					LOG.error("Het object dat terug kwam als AuthRequest (van het type " + extensionRequestObject.getClass().getName() + ") is niet van het verwachtte type "
						+ SRegRequest.class.getName());
				}
			}

			if (authRequest.hasExtension(AxMessage.OPENID_NS_AX))
			{
				LOG.debug("Verwerken AX Verzoek");
				MessageExtension extensionRequestObject = authRequest.getExtension(AxMessage.OPENID_NS_AX);
				FetchResponse fetchResponse = null;
				Map<String, String> axData = new HashMap<String, String>();
				if (extensionRequestObject instanceof FetchRequest)
				{
					FetchRequest axRequest = (FetchRequest) extensionRequestObject;
					ParameterList parameters = axRequest.getParameters();
					fetchResponse = FetchResponse.createFetchResponse(axRequest, axData);
					if (parameters.hasParameter("type.email"))
					{
						axData.put("email", openIdWrapper.getEmailadres());
						fetchResponse.addAttribute("email", "http://axschema.org/contact/email", openIdWrapper.getEmailadres());
					}
					if (parameters.hasParameter("type.fullname"))
					{
						axData.put("fullname", openIdWrapper.getVolledigenaam());
						fetchResponse.addAttribute("fullname", "http://www.topicus.nl/colonis/ax/fullname", openIdWrapper.getVolledigenaam());
					}
					if (parameters.hasParameter("type.medewerkerId"))
					{
						axData.put("medewerkerId", openIdWrapper.getMedewerkerId().toString());
						fetchResponse.addAttribute("medewerkerId", "http://www.topicus.nl/colonis/ax/medewerkerId", openIdWrapper.getMedewerkerId().toString());
					}
					if (parameters.hasParameter("type.organisatieId"))
					{
						axData.put("organisatieId", openIdWrapper.getOrganisatieId().toString());
						fetchResponse.addAttribute("organisatieId", "http://www.topicus.nl/colonis/ax/organisatieId", openIdWrapper.getOrganisatieId().toString());
					}
					if (parameters.hasParameter("type.organisatieType"))
					{
						axData.put("organisatieType", openIdWrapper.getOrganisatieType());
						fetchResponse.addAttribute("organisatieType", "http://www.topicus.nl/colonis/ax/organisatieType", openIdWrapper.getOrganisatieType());
					}
					if (parameters.hasParameter("type.rechtniveau"))
					{
						axData.put("rechtniveau", openIdWrapper.getToegangslevel());
						fetchResponse.addAttribute("rechtniveau", "http://www.topicus.nl/colonis/ax/rechtniveau", openIdWrapper.getToegangslevel());
					}
					if (parameters.hasParameter("type.zichtbareOrganisaties"))
					{
						axData.put("zichtbareOrganisaties", openIdWrapper.getAlleZichtbareOrganisaties());
						fetchResponse.addAttribute("zichtbareOrganisaties", "http://www.topicus.nl/colonis/ax/zichtbareOrganisaties", openIdWrapper.getAlleZichtbareOrganisaties());
					}
					authResponse.addExtension(fetchResponse);
				}
				else
				{
					LOG.error("AX verwerking gestopt. Het object van het AuthRequest (van het type " + extensionRequestObject.getClass().getName()
						+ ") is niet van het verwachtte type " + AxMessage.class.getName());
				}
			}
			if (!(authResponse instanceof DirectError))
			{
				serverManager.sign((AuthSuccess) authResponse);
			}
		}

		LOG.trace("Klaar met maken autorisatie reactie");
		return authResponse;
	}
}
