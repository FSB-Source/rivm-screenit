package nl.rivm.screenit.service.impl;

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

import java.util.ArrayList;
import java.util.List;

import nl.rivm.screenit.PreferenceKey;
import nl.rivm.screenit.dao.HuisartsBerichtTemplateDao;
import nl.rivm.screenit.edi.model.MedVryOut;
import nl.rivm.screenit.edi.model.OutboundMessageData;
import nl.rivm.screenit.edi.service.EdiMessageService;
import nl.rivm.screenit.model.GbaPersoon;
import nl.rivm.screenit.model.Gebruiker;
import nl.rivm.screenit.model.HuisartsBericht;
import nl.rivm.screenit.model.HuisartsBerichtTemplate;
import nl.rivm.screenit.model.Instelling;
import nl.rivm.screenit.model.InstellingGebruiker;
import nl.rivm.screenit.model.MailMergeContext;
import nl.rivm.screenit.model.MailVerzenden;
import nl.rivm.screenit.model.Rivm;
import nl.rivm.screenit.model.ScreeningOrganisatie;
import nl.rivm.screenit.model.enums.HuisartsBerichtType;
import nl.rivm.screenit.model.enums.MergeField;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.LogService;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.patientregistratie.persoonsgegevens.model.NaamGebruik;
import nl.topicuszorg.patientregistratie.persoonsgegevens.model.Patient;
import nl.topicuszorg.patientregistratie.persoonsgegevens.model.Persoon;
import nl.topicuszorg.preferencemodule.service.SimplePreferenceService;

import org.apache.commons.lang.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;

public abstract class EdiServiceBaseImpl
{
	private static final Logger LOG = LoggerFactory.getLogger(EdiServiceBaseImpl.class);

	@Autowired
	protected EdiMessageService ediMessageService;

	@Autowired
	protected ICurrentDateSupplier currentDateSupplier;

	@Autowired
	protected HibernateService hibernateService;

	@Autowired
	protected LogService logService;

	@Autowired
	protected SimplePreferenceService simplePreferenceService;

	@Autowired
	private HuisartsBerichtTemplateDao templateDao;

	@Autowired
	@Qualifier("afzendEmailadres")
	private String afzendEmailadres;

	protected MedVryOut maakMedVry(HuisartsBericht huisartsBericht)
	{
		LOG.debug("Er wordt een EDI bericht gemaakt voor HuisartsBericht met ID: " + huisartsBericht.getId());
		MedVryOut medVryOut = new MedVryOut();
		medVryOut.setDatum(currentDateSupplier.getDate());
		medVryOut.setMagBsnGebruiken(true);
		return medVryOut;
	}

	protected void zetPatient(HuisartsBericht huisartsBericht, MedVryOut medVryOut)
	{
		GbaPersoon persoon = huisartsBericht.getClient().getPersoon();
		Persoon transientPersoon = new Persoon(persoon);
		transientPersoon.setBsnControleDatum(currentDateSupplier.getDate());
		transientPersoon.setBsnGeverifieerd(true);
		NaamGebruik naamGebruik = transientPersoon.getNaamGebruik();
		if (naamGebruik != null && NaamGebruik.EIGEN == naamGebruik)
		{
			transientPersoon.setPartnerAchternaam(null);
			transientPersoon.setPartnerTussenvoegsel(null);
		}
		else if (naamGebruik != null && NaamGebruik.PARTNER == naamGebruik)
		{
			if (StringUtils.isNotBlank(transientPersoon.getPartnerAchternaam()))
			{
				transientPersoon.setAchternaam(transientPersoon.getPartnerAchternaam());
				transientPersoon.setTussenvoegsel(transientPersoon.getPartnerTussenvoegsel());
				transientPersoon.setPartnerAchternaam(null);
				transientPersoon.setPartnerTussenvoegsel(null);
			}
			else
			{
				transientPersoon.setPartnerAchternaam(null);
				transientPersoon.setPartnerTussenvoegsel(null);
			}
		}

		Patient<Persoon> transientPatient = new Patient<Persoon>();
		transientPatient.setPersoon(transientPersoon);
		medVryOut.setPatient(transientPatient);
	}

	protected InstellingGebruiker zetZender(HuisartsBericht huisartsBericht, MedVryOut medVry)
	{
		InstellingGebruiker sender = new InstellingGebruiker();
		ScreeningOrganisatie so = huisartsBericht.getScreeningsOrganisatie();
		sender.setMedewerker(new Gebruiker());
		sender.getMedewerker().setAchternaam(so.getNaam());
		sender.getMedewerker().setVoornaam("SO");
		sender.setEmail(so.getEnovationEdiAdres());
		medVry.setSenderId(so.getEnovationKlantnummer());
		return sender;
	}

	protected void zetInhoud(String berichtInhoud, HuisartsBerichtType berichtType, MedVryOut medVryOut, String transactionId)
	{
		medVryOut.setSubject("[" + transactionId + "] " + berichtType.getNaam());
		String tekstString = "";
		if (StringUtils.isNotEmpty(berichtInhoud))
		{
			tekstString = berichtInhoud;
		}
		medVryOut.setVrijetekst(tekstString);
	}

	protected String getAlgemeneLoggingTekst(HuisartsBerichtType berichtType, String foutmelding, String afzender, String ontvanger, StringBuilder logtekst)
	{
		if (berichtType != null)
		{
			logtekst.append("Type: ");
			logtekst.append(berichtType.getNaam());
			logtekst.append(", ");
		}
		if (StringUtils.isNotBlank(foutmelding))
		{
			logtekst.append("Foutmelding: ");
			logtekst.append(foutmelding);
			logtekst.append(", ");
		}
		if (StringUtils.isNotBlank(afzender))
		{
			logtekst.append("Afzender: ");
			logtekst.append(afzender);
			logtekst.append(", ");
		}
		if (StringUtils.isNotBlank(ontvanger))
		{
			logtekst.append("Ontvanger: ");
			logtekst.append(ontvanger);
			logtekst.append(", ");
		}
		return StringUtils.removeEnd(logtekst.toString(), ", ");
	}

	protected String verzendCheck(MedVryOut medVryOut, ScreeningOrganisatie so)
	{
		String foutmelding = "";
		if (StringUtils.isBlank(medVryOut.getVrijetekst()))
		{
			foutmelding += "Er is geen bericht inhoud, dit betekend dat er geen bericht template gedefineerd is. ";
		}
		if (StringUtils.isBlank(so.getEnovationEdiAdres()))
		{
			foutmelding += "Er is geen Enovation EDI-adres ingevoerd bij de SO. Dit is vereist om een bericht te kunnen verzenden. SO: " + so.getNaam() + ". ";
		}
		if (StringUtils.isBlank(so.getEnovationKlantnummer()))
		{
			foutmelding += "Er is geen Enovation klantnummer ingevoerd bij de SO. Dit is vereist om een bericht te kunnen verzenden. SO: " + so.getNaam() + ". ";
		}
		return foutmelding;
	}

	protected MailVerzenden manipulateEmailadressen(InstellingGebruiker sender, OutboundMessageData<MedVryOut> outboundMessageData)
	{
		MailVerzenden mailVerzendOptie = simplePreferenceService.getEnum(PreferenceKey.MAIL_VERZENDEN.toString(), MailVerzenden.class);
		if (mailVerzendOptie == null)
		{
			mailVerzendOptie = MailVerzenden.AAN;
		}
		if (MailVerzenden.ALTERNATIEF_ADRES.equals(mailVerzendOptie))
		{
			outboundMessageData.setSubject(outboundMessageData.getSubject() + " (Orig. TO: " + outboundMessageData.getAddress() + ", orig. FROM: " + sender.getEmail() + ")");
			outboundMessageData.setAddress(simplePreferenceService.getString(PreferenceKey.ALTERNATIEF_ADRES.name()));
			sender.setEmail(afzendEmailadres);
			LOG.info("Er wordt een huisartsbericht naar het alternatieve email adres gestuurd.");
		}
		return mailVerzendOptie;
	}

	protected String merge(MailMergeContext context, HuisartsBerichtType berichtType)
	{
		String berichtInhoud = "";
		HuisartsBerichtTemplate template = templateDao.getTemplateByType(berichtType);
		if (template != null)
		{
			berichtInhoud = template.getBerichtInhoud();
		}
		else
		{
			throw new IllegalStateException("Er is geen template beschikbaar voor HuisartsBerichtType: " + berichtType.getNaam());
		}
		for (MergeField mergeField : MergeField.values())
		{
			String searchString = "{" + mergeField.getFieldName() + "}";
			if (StringUtils.indexOf(berichtInhoud, searchString) > -1)
			{
				String replacement = "";
				if (mergeField.inHuisartsenbericht())
				{
					Object value = mergeField.getValue(context);
					if (value != null)
					{
						replacement = value.toString();
					}
				}
				berichtInhoud = StringUtils.replace(berichtInhoud, searchString, replacement);
			}
		}
		return berichtInhoud;
	}

	protected List<Instelling> addRivmInstelling(List<Instelling> instellingen)
	{
		List<Rivm> rivm = hibernateService.loadAll(Rivm.class);
		List<Instelling> rivmInstellingen = new ArrayList<>(rivm);
		instellingen.addAll(rivmInstellingen);
		return instellingen;
	}
}
