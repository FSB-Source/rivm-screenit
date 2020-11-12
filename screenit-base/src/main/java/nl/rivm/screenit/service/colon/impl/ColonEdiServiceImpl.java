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

import nl.rivm.screenit.edi.model.MedVryOut;
import nl.rivm.screenit.edi.model.OutboundMessageData;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.EnovationHuisarts;
import nl.rivm.screenit.model.Gemeente;
import nl.rivm.screenit.model.InstellingGebruiker;
import nl.rivm.screenit.model.MailMergeContext;
import nl.rivm.screenit.model.MailVerzenden;
import nl.rivm.screenit.model.MedVryOntvanger;
import nl.rivm.screenit.model.OnbekendeHuisarts;
import nl.rivm.screenit.model.ScreeningOrganisatie;
import nl.rivm.screenit.model.colon.ColonHuisartsBericht;
import nl.rivm.screenit.model.colon.ColonHuisartsBerichtStatus;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.HuisartsBerichtType;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.service.colon.ColonEdiService;
import nl.rivm.screenit.service.impl.EdiServiceBaseImpl;
import nl.rivm.screenit.util.NaamUtil;

import org.apache.commons.lang.StringUtils;
import org.joda.time.DateTime;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Service
@Transactional(propagation = Propagation.SUPPORTS)
public class ColonEdiServiceImpl extends EdiServiceBaseImpl implements ColonEdiService
{
	private static final Logger LOG = LoggerFactory.getLogger(ColonEdiServiceImpl.class);

	@Override
	public void verstuurMedVry(ColonHuisartsBericht huisartsBericht)
	{
		if (huisartsBericht != null)
		{
			String transactionId = Long.toString(currentDateSupplier.getDate().getTime());

			MedVryOut medVry = maakMedVry(huisartsBericht);
			zetPatient(huisartsBericht, medVry);
			zetOntvanger(huisartsBericht, medVry);
			zetInhoud(huisartsBericht.getBerichtInhoud(), huisartsBericht.getBerichtType(), medVry, transactionId);
			InstellingGebruiker sender = zetZender(huisartsBericht, medVry);
			verstuur(huisartsBericht, transactionId, medVry, sender);
		}
	}

	@Override
	public ColonHuisartsBericht maakHuisartsBericht(HuisartsBerichtType berichtType, ColonHuisartsBerichtStatus status, Client client, EnovationHuisarts huisarts,
		OnbekendeHuisarts onbekendeHuisarts, MailMergeContext context, boolean opnieuwVerzonden)
	{
		if (client.getPersoon().getOverlijdensdatum() != null)
		{
			LOG.debug("Er wordt geen HuisartsBericht gemaakt voor het HuisartsBerichtType: " + berichtType.getNaam() + ", met de status: " + status.name() + ", voor Client: "
				+ client.getId() + ". Client is overleden.");
			return null;
		}
		LOG.debug("Er wordt een HuisartsBericht gemaakt voor het HuisartsBerichtType: " + berichtType.getNaam() + ", met de status: " + status.name() + ", voor Client: "
			+ client.getId());
		DateTime date = currentDateSupplier.getDateTime();

		ColonHuisartsBericht haBericht = new ColonHuisartsBericht();
		haBericht.setBerichtType(berichtType);
		haBericht.setAanmaakDatum(date.toDate());
		haBericht.setStatus(status);
		haBericht.setHuisarts(huisarts);
		haBericht.setOnbekendeHuisarts(onbekendeHuisarts);
		haBericht.setClient(client);
		haBericht.setOpnieuwVerzonden(opnieuwVerzonden);
		Gemeente gbaGemeente = client.getPersoon().getGbaAdres().getGbaGemeente();
		ScreeningOrganisatie screeningOrganisatie = gbaGemeente.getScreeningOrganisatie();
		if (screeningOrganisatie == null)
		{
			throw new IllegalStateException(
				"Client met id " + client.getId() + " is aan gemeente " + gbaGemeente.getNaam()
					+ " gekoppeld. Alleen gemeente is niet gekoppeld aan een screeningsorganisatie/regio");
		}
		haBericht.setScreeningsOrganisatie(screeningOrganisatie);
		haBericht.setScreeningsRonde(client.getColonDossier().getLaatsteScreeningRonde());

		String berichtInhoud = merge(context, berichtType);
		haBericht.setBerichtInhoud(berichtInhoud);

		client.getHuisartsBerichten().add(haBericht);
		hibernateService.saveOrUpdate(haBericht);
		hibernateService.saveOrUpdate(client);

		return haBericht;
	}

	private void verstuur(ColonHuisartsBericht huisartsBericht, String transactionId, MedVryOut medVry, InstellingGebruiker sender)
	{
		OutboundMessageData<MedVryOut> outboundMessageData = new OutboundMessageData<>(medVry);
		outboundMessageData.setSubject(medVry.getSubject());
		outboundMessageData.setAddress(medVry.getMail());

		String foutmelding = verzendCheck(medVry, huisartsBericht.getScreeningsOrganisatie());

		try
		{
			MailVerzenden mailVerzenden = manipulateEmailadressen(sender, outboundMessageData);
			if (StringUtils.isBlank(foutmelding)
				&& (MailVerzenden.UIT.equals(mailVerzenden) || ediMessageService.sendMedVry(sender, sender.getEmail(), outboundMessageData, transactionId)))
			{
				huisartsBericht.setStatus(ColonHuisartsBerichtStatus.VERZENDEN_GELUKT);
				huisartsBericht.setVerzendDatum(currentDateSupplier.getDate());
				huisartsBericht.setBerichtInhoud("");
				LOG.debug("Er is succesvol een EDI bericht verzonden voor HuisartsBericht met ID: " + huisartsBericht.getId());
			}
			else
			{
				huisartsBericht.setStatus(ColonHuisartsBerichtStatus.VERZENDEN_MISLUKT);
				LOG.error("Het is niet gelukt een EDI bericht verzonden voor HuisartsBericht met ID: " + huisartsBericht.getId() + ". " + foutmelding);
			}
		}
		catch (Exception e) 
		{
			huisartsBericht.setStatus(ColonHuisartsBerichtStatus.VERZENDEN_MISLUKT);
			LOG.error("Het is niet gelukt een EDI bericht verzonden voor HuisartsBericht met ID: " + huisartsBericht.getId() + ". " + e.getMessage());
		}
		finally
		{
			hibernateService.save(huisartsBericht);

			if (ColonHuisartsBerichtStatus.VERZENDEN_GELUKT == huisartsBericht.getStatus())
			{
				if (Boolean.TRUE.equals(huisartsBericht.isOpnieuwVerzonden()))
				{
					logService.logGebeurtenis(LogGebeurtenis.HUISARTSBERICHT_OPNIEUW_VERSTUURD, huisartsBericht.getClient(),
						getLoggingTekst(huisartsBericht, foutmelding, huisartsBericht.getScreeningsOrganisatie().getEnovationEdiAdres(), medVry.getReceiverId()),
						Bevolkingsonderzoek.COLON);
				}
				else
				{
					logService.logGebeurtenis(LogGebeurtenis.HUISARTS_BERICHT_VERZONDEN, huisartsBericht.getClient(),
						getLoggingTekst(huisartsBericht, foutmelding, huisartsBericht.getScreeningsOrganisatie().getEnovationEdiAdres(), medVry.getReceiverId()),
						Bevolkingsonderzoek.COLON);
				}
			}
			else if (ColonHuisartsBerichtStatus.VERZENDEN_MISLUKT == huisartsBericht.getStatus())
			{
				logService.logGebeurtenis(LogGebeurtenis.HUISARTS_BERICHT_NIET_VERZONDEN, huisartsBericht.getClient(),
					getLoggingTekst(huisartsBericht, foutmelding, huisartsBericht.getScreeningsOrganisatie().getEnovationEdiAdres(), medVry.getReceiverId()),
					Bevolkingsonderzoek.COLON);
			}
		}
	}

	private void zetOntvanger(ColonHuisartsBericht huisartsBericht, MedVryOut medVryOut)
	{
		MedVryOntvanger ontvanger = new MedVryOntvanger(huisartsBericht, "oss.direct@lms.lifeline.nl");
		medVryOut.setOntvanger(ontvanger);
		if (huisartsBericht.getHuisarts() != null)
		{
			medVryOut.setReceiverId(huisartsBericht.getHuisarts().getKlantnummer());
		}
		else if (huisartsBericht.getOnbekendeHuisarts() != null)
		{
			medVryOut.setReceiverId("postbezorging");
		}
		medVryOut.setMail(ontvanger.getEdiMailAdres());
	}

	private String getLoggingTekst(ColonHuisartsBericht haBericht, String foutmelding, String afzender, String ontvanger)
	{
		StringBuilder logtekst = new StringBuilder();
		if (haBericht.getHuisarts() != null)
		{
			logtekst.append("Huisarts: ");
			logtekst.append(NaamUtil.getNaamHuisarts(haBericht.getHuisarts()));
			logtekst.append(", ");
		}
		else if (haBericht.getOnbekendeHuisarts() != null)
		{
			logtekst.append("Onbekende huisarts: ");
			logtekst.append(NaamUtil.getNaamOnbekendeHuisarts(haBericht.getOnbekendeHuisarts()));
			logtekst.append(", ");
		}
		return getAlgemeneLoggingTekst(haBericht.getBerichtType(), foutmelding, afzender, ontvanger, logtekst);
	}
}
