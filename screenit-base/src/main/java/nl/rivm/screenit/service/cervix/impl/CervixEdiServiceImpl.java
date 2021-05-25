package nl.rivm.screenit.service.cervix.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
 * %%
 * Copyright (C) 2012 - 2021 Facilitaire Samenwerking Bevolkingsonderzoek
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

import java.util.ArrayList;
import java.util.List;

import nl.rivm.screenit.edi.model.MedVryOut;
import nl.rivm.screenit.edi.model.OutboundMessageData;
import nl.rivm.screenit.huisartsenportaal.enums.CervixLocatieStatus;
import nl.rivm.screenit.model.Account;
import nl.rivm.screenit.model.Instelling;
import nl.rivm.screenit.model.InstellingGebruiker;
import nl.rivm.screenit.model.MailMergeContext;
import nl.rivm.screenit.model.MailVerzenden;
import nl.rivm.screenit.model.MedVryOntvanger;
import nl.rivm.screenit.model.cervix.CervixHuisartsBericht;
import nl.rivm.screenit.model.cervix.CervixHuisartsLocatie;
import nl.rivm.screenit.model.cervix.CervixUitstrijkje;
import nl.rivm.screenit.model.cervix.enums.CervixHuisartsBerichtStatus;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.HuisartsBerichtType;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.service.cervix.CervixEdiService;
import nl.rivm.screenit.service.cervix.enums.CervixEdiVerstuurStatus;
import nl.rivm.screenit.service.impl.EdiServiceBaseImpl;
import nl.rivm.screenit.util.NaamUtil;

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
public class CervixEdiServiceImpl extends EdiServiceBaseImpl implements CervixEdiService
{
	private static final Logger LOG = LoggerFactory.getLogger(CervixEdiServiceImpl.class);

	@Autowired
	@Qualifier(value = "ossAfleverAdres")
	private String ossAfleverAdres;

	@Override
	public void verstuurMedVry(CervixHuisartsBericht huisartsBericht, Account loggedInAccount)
	{
		CervixHuisartsLocatie locatie = huisartsBericht.getHuisartsLocatie();
		if (klantnummerNietGeverifieerd(locatie))
		{
			huisartsBericht.setStatus(CervixHuisartsBerichtStatus.KLANTNUMMER_NIET_GEVERIFIEERD);
			hibernateService.saveOrUpdate(huisartsBericht);
			return;
		}

		String berichtInhoud = maakBerichtInhoud(huisartsBericht);

		String transactionId = Long.toString(currentDateSupplier.getDate().getTime());

		MedVryOut medVry = maakMedVry(huisartsBericht);
		zetPatient(huisartsBericht, medVry);
		InstellingGebruiker sender = zetZender(huisartsBericht, medVry);
		zetInhoud(berichtInhoud, huisartsBericht.getBerichtType(), medVry, transactionId);
		zetOntvanger(medVry, locatie);

		String foutmelding = verstuur(huisartsBericht, transactionId, medVry, sender);

		updateHuisartsBerichtNaVerzenden(huisartsBericht, berichtInhoud, StringUtils.isBlank(foutmelding));

		LogGebeurtenis logGebeurtenis = bepaalLoggebeurtenisVoorHuisartsBericht(huisartsBericht);
		String melding = getLoggingTekst(huisartsBericht.getHuisartsLocatie(), huisartsBericht.getBerichtType(), foutmelding,
			huisartsBericht.getScreeningsOrganisatie().getEnovationEdiAdres(), medVry.getReceiverId());
		schrijfLogGebeurtenis(logGebeurtenis, huisartsBericht, melding, loggedInAccount);
	}

	@Override
	public CervixEdiVerstuurStatus verstuurMedVryNaarExtraHuisartsLocatie(CervixHuisartsBericht huisartsBericht, CervixHuisartsLocatie extraLocatie, Account loggedInAccount)
	{
		if (klantnummerNietGeverifieerd(extraLocatie))
		{
			return CervixEdiVerstuurStatus.KLANTNUMMER_NIET_GEVERIFIEERD;
		}

		String transactionId = Long.toString(currentDateSupplier.getDate().getTime());

		MedVryOut medVry = maakMedVry(huisartsBericht);
		zetPatient(huisartsBericht, medVry);
		InstellingGebruiker sender = zetZender(huisartsBericht, medVry);
		zetInhoud(maakBerichtInhoud(huisartsBericht), huisartsBericht.getBerichtType(), medVry, transactionId);
		zetOntvanger(medVry, extraLocatie);

		String foutmelding = verstuur(huisartsBericht, transactionId, medVry, sender);
		boolean succesvol = StringUtils.isBlank(foutmelding);

		updateHuisartsBerichtNaVerzendenExtraHuisarts(huisartsBericht, extraLocatie, succesvol);

		LogGebeurtenis logGebeurtenis = succesvol ? LogGebeurtenis.HUISARTSBERICHT_OPNIEUW_VERSTUURD : LogGebeurtenis.HUISARTS_BERICHT_NIET_VERZONDEN;
		String melding = getLoggingTekst(extraLocatie, huisartsBericht.getBerichtType(), foutmelding,
			huisartsBericht.getScreeningsOrganisatie().getEnovationEdiAdres(), medVry.getReceiverId());
		schrijfLogGebeurtenis(logGebeurtenis, huisartsBericht, melding, loggedInAccount);

		return succesvol ? CervixEdiVerstuurStatus.VERSTUURD : CervixEdiVerstuurStatus.VERSTUREN_MISLUKT;
	}

	@Override
	public void verstuurKlantnummerVerificatieMedVry(CervixHuisartsBericht huisartsBericht)
	{
		MailMergeContext context = new MailMergeContext();
		context.putValue(MailMergeContext.CONTEXT_HA_LOCATIE, huisartsBericht.getHuisartsLocatie());
		context.putValue(MailMergeContext.CONTEXT_CERVIX_HUISARTS, huisartsBericht.getHuisartsLocatie().getHuisarts());
		huisartsBericht.setBerichtInhoud(merge(context, huisartsBericht.getBerichtType()));

		String transactionId = Long.toString(currentDateSupplier.getDate().getTime());

		MedVryOut medVry = maakMedVry(huisartsBericht);
		zetOntvanger(medVry, huisartsBericht.getHuisartsLocatie());
		zetInhoud(huisartsBericht.getBerichtInhoud(), huisartsBericht.getBerichtType(), medVry, transactionId);
		InstellingGebruiker sender = zetZender(huisartsBericht, medVry);
		String foutmelding = verstuur(huisartsBericht, transactionId, medVry, sender);

		LogGebeurtenis logGebeurtenis = StringUtils.isBlank(foutmelding) ? LogGebeurtenis.CERVIX_ZORGMAIL_VERIFICATIE_HUISARTSBERICHT_VERSTUURD
			: LogGebeurtenis.CERVIX_ZORGMAIL_VERIFICATIE_HUISARTSBERICHT_VERSTUREN_MISLUKT;
		String melding = getLoggingTekst(huisartsBericht.getHuisartsLocatie(), huisartsBericht.getBerichtType(), foutmelding,
			huisartsBericht.getScreeningsOrganisatie().getEnovationEdiAdres(), medVry.getReceiverId());
		schrijfLogGebeurtenis(logGebeurtenis, huisartsBericht, melding, null);
	}

	private boolean klantnummerNietGeverifieerd(CervixHuisartsLocatie locatie)
	{
		return CervixLocatieStatus.KLANTNUMMER_NIET_GEVERIFIEERD.equals(locatie.getStatus())
			|| Boolean.TRUE.equals(locatie.getMoetVerifierenVoorActivatie()) && CervixLocatieStatus.INACTIEF.equals(locatie.getStatus());
	}

	private String maakBerichtInhoud(CervixHuisartsBericht huisartsBericht)
	{
		MailMergeContext context = new MailMergeContext();

		CervixUitstrijkje uitstrijkje;
		if (huisartsBericht.getUitstrijkje() != null)
		{
			uitstrijkje = huisartsBericht.getUitstrijkje();
		}
		else
		{
			uitstrijkje = huisartsBericht.getLabformulier().getUitstrijkje();
			context.setBmhkLaboratorium(huisartsBericht.getLabformulier().getLaboratorium());
		}

		context.setClient(huisartsBericht.getClient());
		context.setCervixUitnodiging(uitstrijkje.getUitnodiging());
		context.setBrief(uitstrijkje.getBrief());

		return merge(context, huisartsBericht.getBerichtType());
	}

	private String verstuur(CervixHuisartsBericht huisartsBericht, String transactionId, MedVryOut medVry, InstellingGebruiker sender)
	{
		OutboundMessageData<MedVryOut> outboundMessageData = new OutboundMessageData<>(medVry);
		outboundMessageData.setSubject(medVry.getSubject());
		outboundMessageData.setAddress(medVry.getMail());

		String foutmelding = verzendCheck(medVry, huisartsBericht.getScreeningsOrganisatie());

		try
		{
			MailVerzenden mailVerzenden = manipulateEmailadressen(sender, outboundMessageData);

			if (StringUtils.isBlank(foutmelding) && !MailVerzenden.UIT.equals(mailVerzenden)
				&& !ediMessageService.sendMedVry(sender, sender.getEmail(), outboundMessageData, transactionId))
			{
				foutmelding = "Probleem met versturen MedVry";
			}
		}
		catch (Exception e)
		{
			foutmelding = e.getMessage();
		}

		if (StringUtils.isBlank(foutmelding))
		{
			LOG.debug("Er is succesvol een EDI bericht verzonden voor HuisartsBericht met ID: " + huisartsBericht.getId());
		}
		else
		{
			LOG.error("Het is niet gelukt een EDI bericht versturen voor HuisartsBericht met ID: " + huisartsBericht.getId() + ". " + foutmelding);
		}

		return foutmelding;
	}

	private void updateHuisartsBerichtNaVerzenden(CervixHuisartsBericht huisartsBericht, String berichtInhoud, boolean verzendenSuccesvol)
	{
		if (verzendenSuccesvol)
		{
			huisartsBericht.setBerichtInhoud("");
			huisartsBericht.setStatus(getNieuweStatusNaSuccesvolVerzenden(huisartsBericht));
		}
		else
		{
			huisartsBericht.setBerichtInhoud(berichtInhoud);
			huisartsBericht.setStatus(getNieuweStatusNaVerzendenMislukt(huisartsBericht));
		}
		huisartsBericht.setStatusDatum(currentDateSupplier.getDate());
		hibernateService.saveOrUpdate(huisartsBericht);
	}

	private CervixHuisartsBerichtStatus getNieuweStatusNaSuccesvolVerzenden(CervixHuisartsBericht huisartsBericht)
	{
		switch (huisartsBericht.getStatus())
		{
		case KLANTNUMMER_NIET_GEVERIFIEERD:
		case AANGEMAAKT:
		case VERSTUREN_MISLUKT:
			return CervixHuisartsBerichtStatus.VERSTUURD;
		case VERSTUURD:
		case OPNIEUW_VERSTUURD:
		case OPNIEUW_VERSTUREN_MISLUKT:
			return CervixHuisartsBerichtStatus.OPNIEUW_VERSTUURD;
		default:
			throw new IllegalStateException();
		}
	}

	private CervixHuisartsBerichtStatus getNieuweStatusNaVerzendenMislukt(CervixHuisartsBericht huisartsBericht)
	{
		switch (huisartsBericht.getStatus())
		{
		case KLANTNUMMER_NIET_GEVERIFIEERD:
		case AANGEMAAKT:
		case VERSTUREN_MISLUKT:
			return CervixHuisartsBerichtStatus.VERSTUREN_MISLUKT;
		case VERSTUURD:
		case OPNIEUW_VERSTUURD:
		case OPNIEUW_VERSTUREN_MISLUKT:
			return CervixHuisartsBerichtStatus.OPNIEUW_VERSTUREN_MISLUKT;
		default:
			throw new IllegalStateException();
		}
	}

	private LogGebeurtenis bepaalLoggebeurtenisVoorHuisartsBericht(CervixHuisartsBericht huisartsBericht)
	{
		switch (huisartsBericht.getStatus())
		{
		case VERSTUURD:
			return LogGebeurtenis.HUISARTS_BERICHT_VERZONDEN;
		case OPNIEUW_VERSTUURD:
			return LogGebeurtenis.HUISARTSBERICHT_OPNIEUW_VERSTUURD;
		case VERSTUREN_MISLUKT:
		case OPNIEUW_VERSTUREN_MISLUKT:
			return LogGebeurtenis.HUISARTS_BERICHT_NIET_VERZONDEN;
		default:
			throw new IllegalStateException();
		}
	}

	private void updateHuisartsBerichtNaVerzendenExtraHuisarts(CervixHuisartsBericht huisartsBericht, CervixHuisartsLocatie locatie, boolean verzendenSuccesvol)
	{
		if (verzendenSuccesvol)
		{
			huisartsBericht.setExtraHuisartsLocatie(locatie);
			huisartsBericht.setExtraHuisartsLocatieVerstuurdDatum(currentDateSupplier.getDate());
			hibernateService.saveOrUpdate(huisartsBericht);
		}
	}

	private void schrijfLogGebeurtenis(LogGebeurtenis logGebeurtenis, CervixHuisartsBericht huisartsBericht, String melding, Account loggedInAccount)
	{
		List<Instelling> dashboardOrganisaties = addRivmInstelling(new ArrayList<>());
		dashboardOrganisaties.add(huisartsBericht.getScreeningsOrganisatie());
		logService.logGebeurtenis(logGebeurtenis, dashboardOrganisaties, loggedInAccount, huisartsBericht.getClient(), melding, Bevolkingsonderzoek.CERVIX);
	}

	private String getLoggingTekst(CervixHuisartsLocatie huisartsLocatie, HuisartsBerichtType berichtType, String foutmelding, String afzender, String ontvanger)
	{
		StringBuilder logtekst = new StringBuilder();
		if (huisartsLocatie != null)
		{
			logtekst.append("Huisarts: ");
			logtekst.append(NaamUtil.getNaamHuisarts(huisartsLocatie.getHuisarts()));
			logtekst.append(", ");
			logtekst.append("Locatie: ");
			logtekst.append(huisartsLocatie.getNaam());
			logtekst.append(", ");
		}
		return getAlgemeneLoggingTekst(berichtType, foutmelding, afzender, ontvanger, logtekst);
	}

	private void zetOntvanger(MedVryOut medVryOut, CervixHuisartsLocatie huisartsLocatie)
	{
		MedVryOntvanger ontvanger = new MedVryOntvanger(huisartsLocatie, ossAfleverAdres);
		medVryOut.setOntvanger(ontvanger);
		medVryOut.setReceiverId(huisartsLocatie.getZorgmailklantnummer());
		medVryOut.setMail(ontvanger.getEdiMailAdres());
	}
}
