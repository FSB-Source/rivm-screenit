package nl.rivm.screenit.service.colon.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
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

import java.util.Date;
import java.util.Iterator;
import java.util.Properties;

import javax.mail.Message;
import javax.mail.MessagingException;
import javax.mail.Session;
import javax.mail.Transport;
import javax.mail.internet.InternetAddress;
import javax.mail.internet.MimeMessage;

import nl.rivm.screenit.PreferenceKey;
import nl.rivm.screenit.dao.colon.HuisartsBerichtDao;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.EnovationHuisarts;
import nl.rivm.screenit.model.Instelling;
import nl.rivm.screenit.model.MailMergeContext;
import nl.rivm.screenit.model.MailVerzenden;
import nl.rivm.screenit.model.OnbekendeHuisarts;
import nl.rivm.screenit.model.ScreeningOrganisatie;
import nl.rivm.screenit.model.colon.ColonHuisartsBericht;
import nl.rivm.screenit.model.colon.ColonHuisartsBerichtStatus;
import nl.rivm.screenit.model.colon.ColonScreeningRonde;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.BezwaarType;
import nl.rivm.screenit.model.enums.HuisartsBerichtType;
import nl.rivm.screenit.service.colon.ColonEdiService;
import nl.rivm.screenit.service.colon.ColonHuisartsBerichtService;
import nl.rivm.screenit.util.AdresUtil;
import nl.rivm.screenit.util.BezwaarUtil;
import nl.rivm.screenit.util.NaamUtil;
import nl.topicuszorg.preferencemodule.service.SimplePreferenceService;

import org.apache.commons.lang.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Service
@Transactional(propagation = Propagation.SUPPORTS)
public class ColonHuisartsBerichtServiceImpl implements ColonHuisartsBerichtService
{

	@Autowired(required = false)
	private HuisartsBerichtDao berichtDao;

	@Autowired
	private SimplePreferenceService simplePreferenceService;

	@Autowired
	private ColonEdiService ediService;

	@Autowired
	@Qualifier(value = "ossAanmeldenAdres")
	private String ossAanmeldenAdres;

	@Autowired
	@Qualifier(value = "smtpAuthUsername")
	private String username;

	@Autowired
	@Qualifier(value = "smtpAuthPassword")
	private String password;

	@Autowired
	@Qualifier(value = "mailRelayIp")
	private String host;

	@Autowired
	@Qualifier(value = "mailRelayPort")
	private Integer port;

	@Autowired
	@Qualifier(value = "smtpOverSsl")
	private Boolean ssl;

	@Autowired
	@Qualifier("afzendEmailadres")
	private String afzendEmailadres;

	private static final Logger LOG = LoggerFactory.getLogger(ColonHuisartsBerichtServiceImpl.class);

	@Override
	@Transactional(propagation = Propagation.SUPPORTS)
	public long countBerichten(ColonHuisartsBericht zoekObject, Instelling regioObject)
	{
		return berichtDao.countBerichten(zoekObject, regioObject);
	}

	@Override
	@Transactional(propagation = Propagation.SUPPORTS)
	public Iterator<? extends ColonHuisartsBericht> searchBerichten(ColonHuisartsBericht zoekObject, Instelling regioObject, String sortProperty, boolean ascending, int first,
		int count, boolean werklijst)
	{
		return berichtDao.searchBerichten(zoekObject, regioObject, sortProperty, ascending, first, count, werklijst);
	}

	@Override
	public void verstuurColonHuisartsBericht(Client client, ColonScreeningRonde colonScreeningRonde, HuisartsBerichtType berichtType, MailMergeContext context)
	{
		verstuurColonHuisartsBericht(client, colonScreeningRonde, berichtType, context, false);
	}

	@Override
	public void verstuurColonHuisartsBericht(Client client, ColonScreeningRonde colonScreeningRonde, HuisartsBerichtType berichtType, MailMergeContext context,
		boolean opnieuwVerzonden)
	{
		if (colonScreeningRonde == null || BezwaarUtil.isBezwaarActiefVoor(client, BezwaarType.GEEN_UITWISSELING_MET_DE_HUISARTS, Bevolkingsonderzoek.COLON))
		{
			return;
		}
		EnovationHuisarts huisarts = colonScreeningRonde.getColonHuisarts();
		OnbekendeHuisarts onbekendeHuisarts = colonScreeningRonde.getOnbekendeHuisarts();
		if (huisarts != null || onbekendeHuisarts != null)
		{
			ColonHuisartsBericht huisartsBericht = ediService.maakHuisartsBericht(berichtType,
				ColonHuisartsBerichtStatus.CONTROLE_NIET_NODIG, client, huisarts, onbekendeHuisarts, context, opnieuwVerzonden);
			ediService.verstuurMedVry(huisartsBericht);
		}
	}

	private MailVerzenden mailVerzenden()
	{
		MailVerzenden mailVerzendenPreference = simplePreferenceService.getEnum(PreferenceKey.MAIL_VERZENDEN.toString(), MailVerzenden.class);
		if (mailVerzendenPreference == null)
		{
			mailVerzendenPreference = MailVerzenden.AAN;
		}
		return mailVerzendenPreference;
	}

	@Override
	public void verzendOnbekendeHuisartsAanmeldenMail(OnbekendeHuisarts onbekendeHuisarts, Client client)
	{
		LOG.debug("Er wordt een 'Aanmelden OnbekendeHuisarts' mail gemaakt voor huisarts " + onbekendeHuisarts.getId());
		StringBuilder subjectSb = new StringBuilder();
		subjectSb.append("Aanmelding onbekende arts ");
		subjectSb.append(NaamUtil.getNaamOnbekendeHuisarts(onbekendeHuisarts));
		subjectSb.append(" (");
		subjectSb.append(client.getPersoon().getBsn());
		subjectSb.append(") ");

		StringBuilder messageSb = new StringBuilder();
		messageSb.append("Huisartsnaam: ");
		messageSb.append(onbekendeHuisarts.getHuisartsNaam());
		messageSb.append(", Praktijknaam: ");
		messageSb.append(onbekendeHuisarts.getPraktijkNaam());
		messageSb.append(", Praktijkadres: ");
		messageSb.append(AdresUtil.getAdres(onbekendeHuisarts));
		messageSb.append(", Telefoonnummer: ");
		messageSb.append(onbekendeHuisarts.getTelefoonnummer());
		messageSb.append(", Faxnummer: ");
		messageSb.append(onbekendeHuisarts.getFaxnummer());

		ScreeningOrganisatie screeningOrganisatie = client.getPersoon().getGbaAdres().getGbaGemeente().getScreeningOrganisatie();
		if (screeningOrganisatie == null)
		{
			throw new IllegalStateException(
				"Client met id " + client.getId() + " is aan gemeente " + client.getPersoon().getGbaAdres().getGbaGemeente()
					+ " gekoppeld. Alleen gemeente is niet gekoppeld aan een screeningsorganisatie/regio");
		}

		String from = screeningOrganisatie.getEnovationEdiAdres();
		try
		{
			LOG.info("Aanmelden OnbekendeHuisarts mail wordt verstuurd voor huisarts " + onbekendeHuisarts.getId());
			MailVerzenden mailVerzenden = mailVerzenden();
			String to = ossAanmeldenAdres;
			if (MailVerzenden.ALTERNATIEF_ADRES.equals(mailVerzenden))
			{
				subjectSb.append(" (Orig. TO: " + to + ", orig. FROM: " + from + ")");
				to = simplePreferenceService.getString(PreferenceKey.ALTERNATIEF_ADRES.name());
				from = afzendEmailadres;
			}
			if (!MailVerzenden.UIT.equals(mailVerzenden))
			{
				mailNaarOnbekendeHuisarts(from, to, subjectSb.toString(), messageSb.toString());
			}
		}
		catch (MessagingException e)
		{
			LOG.error("Mail met details over onbekende huisarts is niet verstuurd: " + e.getMessage());
		}
	}

	private boolean mailNaarOnbekendeHuisarts(String from, String to, String subject, String message)
		throws MessagingException
	{
		if (StringUtils.isBlank(to))
		{
			throw new IllegalArgumentException("To-adres mag niet leeg zijn");
		}
		if (ssl == null)
		{
			ssl = false;
		}

		Properties props = new Properties();
		props.setProperty("mail.transport.protocol", "smtp");
		if (port != null)
		{
			props.setProperty("mail.smtp.port", port.toString());
		}

		if (username == null)
		{
			props.setProperty("mail.smtps.auth", "false");
		}
		else
		{
			props.setProperty("mail.smtps.auth", "true");
		}

		if (ssl)
		{
			props.setProperty("mail.smtps.ssl.enable", "true");

		}
		else
		{
			props.setProperty("mail.smtp.ssl.enable", "false");
		}
		props.setProperty("mail.host", host);
		Session session = Session.getInstance(props);

		try
		{
			MimeMessage msg = new MimeMessage(session);
			if (from == null)
			{
				throw new IllegalArgumentException("No From specified!");
			}
			msg.setFrom(new InternetAddress(from));
			InternetAddress[] address = { new InternetAddress(to) };
			msg.setRecipients(Message.RecipientType.TO, address);
			msg.setSubject(subject);
			msg.setSentDate(new Date());
			msg.setText(message, "UTF-8");

			Transport transport = null;
			try
			{
				if (ssl)
				{
					transport = session.getTransport("smtps");
					if (username == null)
					{
						transport.connect();
					}
					else
					{
						transport.connect(host, username, password);
					}

					transport.sendMessage(msg, msg.getAllRecipients());
				}
				else
				{
					transport = session.getTransport("smtp");
					transport.connect(host, port, null, null);
					transport.sendMessage(msg, msg.getAllRecipients());
				}
				return true;
			}
			finally
			{
				if (transport != null)
				{
					transport.close();
				}
			}
		}
		catch (MessagingException mex)
		{
			LOG.error("Er is een fout opgetreden! " + mex.getMessage(), mex);
			Exception ex = mex.getNextException();
			if (ex != null)
			{
				LOG.error(ex.getMessage(), mex);
			}
		}
		return false;
	}

}
