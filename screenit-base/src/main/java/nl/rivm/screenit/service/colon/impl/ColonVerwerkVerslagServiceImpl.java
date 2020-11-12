
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

import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Date;
import java.util.List;
import java.util.Set;

import nl.rivm.screenit.Constants;
import nl.rivm.screenit.dao.VerslagDao;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.DossierStatus;
import nl.rivm.screenit.model.Instelling;
import nl.rivm.screenit.model.InstellingGebruiker;
import nl.rivm.screenit.model.OrganisatieType;
import nl.rivm.screenit.model.ScreeningRondeStatus;
import nl.rivm.screenit.model.berichten.Verslag;
import nl.rivm.screenit.model.berichten.enums.VerslagGeneratie;
import nl.rivm.screenit.model.colon.ColonDossier;
import nl.rivm.screenit.model.colon.ColonScreeningRonde;
import nl.rivm.screenit.model.colon.ColonVerslag;
import nl.rivm.screenit.model.colon.Complicatie;
import nl.rivm.screenit.model.colon.IFOBTTest;
import nl.rivm.screenit.model.colon.MdlVerslag;
import nl.rivm.screenit.model.colon.PaLaboratorium;
import nl.rivm.screenit.model.colon.PaVerslag;
import nl.rivm.screenit.model.colon.enums.ColonUitnodigingsintervalType;
import nl.rivm.screenit.model.colon.enums.MdlVervolgbeleid;
import nl.rivm.screenit.model.colon.verslag.mdl.MdlColoscopieMedischeObservatie;
import nl.rivm.screenit.model.colon.verslag.mdl.MdlDefinitiefVervolgbeleidVoorBevolkingsonderzoekg;
import nl.rivm.screenit.model.colon.verslag.mdl.MdlIncidentcomplicatie;
import nl.rivm.screenit.model.colon.verslag.mdl.MdlLaesiecoloscopiecentrum;
import nl.rivm.screenit.model.colon.verslag.mdl.MdlPoliep;
import nl.rivm.screenit.model.colon.verslag.mdl.MdlTnummerPathologieVerslag;
import nl.rivm.screenit.model.colon.verslag.mdl.MdlVerslagContent;
import nl.rivm.screenit.model.colon.verslag.pa.PaPathologieProtocolColonbioptperPoliep;
import nl.rivm.screenit.model.colon.verslag.pa.PaVerslagContent;
import nl.rivm.screenit.model.enums.ComplicatieErnst;
import nl.rivm.screenit.model.enums.ComplicatieMoment;
import nl.rivm.screenit.model.enums.ComplicatieSoort;
import nl.rivm.screenit.model.formulieren.IdentifierElement;
import nl.rivm.screenit.model.verslag.DSValue;
import nl.rivm.screenit.model.verslag.Quantity;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.colon.ColonDossierBaseService;
import nl.rivm.screenit.service.colon.ColonVerwerkVerslagService;
import nl.rivm.screenit.service.colon.ComplicatieService;
import nl.rivm.screenit.util.ColonScreeningRondeUtil;
import nl.rivm.screenit.util.DateUtil;
import nl.rivm.screenit.util.RomanNumeral;
import nl.topicuszorg.formulieren2.api.definitie.VraagDefinitie;
import nl.topicuszorg.formulieren2.api.resultaat.Antwoord;
import nl.topicuszorg.formulieren2.persistence.resultaat.BooleanAntwoord;
import nl.topicuszorg.formulieren2.persistence.resultaat.DateAntwoord;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;

import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang.StringUtils;
import org.apache.commons.lang3.math.NumberUtils;
import org.joda.time.DateTime;
import org.joda.time.MutableDateTime;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.support.PropertyComparator;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Service
@Transactional(propagation = Propagation.SUPPORTS, readOnly = true)
public class ColonVerwerkVerslagServiceImpl implements ColonVerwerkVerslagService
{

	private static final Logger LOG = LoggerFactory.getLogger(ColonVerwerkVerslagServiceImpl.class);

	private static final List<String> SURVAILLANCE_CODES = Arrays.asList("12", "13", "14", "15");

	@Autowired
	private HibernateService hibernateService;

	@Autowired
	private ColonDossierBaseService dossierBaseService;

	@Autowired
	private VerslagDao verslagDao;

	@Autowired
	private ICurrentDateSupplier currentDateSupplier;

	@Autowired
	private ComplicatieService complicatieService;

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public void verwerkInDossier(MdlVerslag verslag)
	{
		verslag.setVervolgbeleid(getVervolgbeleid(verslag));
		hibernateService.saveOrUpdate(verslag);

		ColonScreeningRonde screeningRonde = verslag.getScreeningRonde();
		Date nu = currentDateSupplier.getDate();
		screeningRonde.setStatusDatum(nu);
		screeningRonde.setDefinitiefVervolgbeleid(null);
		ColonDossier dossier = screeningRonde.getDossier();

		if (dossier.getLaatsteScreeningRonde().equals(screeningRonde))
		{
			MdlVervolgbeleid actueelsteVervolgbeleid = getActueelsteVervolgbeleid(screeningRonde);
			if (actueelsteVervolgbeleid != null && MdlVervolgbeleid.isDefinitief(actueelsteVervolgbeleid))
			{
				if (!ScreeningRondeStatus.AFGEROND.equals(screeningRonde.getStatus()))
				{
					screeningRonde.setStatus(ScreeningRondeStatus.AFGEROND);
					screeningRonde.setAfgerondReden("definitieve diagnose");
				}
				if (dossier.getStatus() == DossierStatus.ACTIEF)
				{
					dossier.setStatus(DossierStatus.INACTIEF);
				}
			}
			else if (Boolean.FALSE.equals(dossier.getAangemeld()))
			{
				dossier.setStatus(DossierStatus.ACTIEF);
				dossier.setInactiveerReden(null);
				dossier.setInactiefVanaf(null);
			}
			hibernateService.saveOrUpdate(screeningRonde);

		}
	}

	private void bepaalEnSetUitnodigingeinterval(MdlVerslag verslag, ColonDossier dossier)
	{
		ColonUitnodigingsintervalType uitnodigingsintervalType = null;
		MdlVervolgbeleid vervolgbeleid = getVervolgbeleid(verslag);
		if (vervolgbeleid == null)
		{
			uitnodigingsintervalType = ColonUitnodigingsintervalType.ENDOSCOPIEVERSLAG_TERUG_BVO;
		}
		else if (vervolgbeleid == MdlVervolgbeleid.SURVEILLANCE)
		{
			uitnodigingsintervalType = ColonUitnodigingsintervalType.ENDOSCOPIEVERSLAG_SURVEILLANCE_1_JAAR;
			MdlDefinitiefVervolgbeleidVoorBevolkingsonderzoekg definitiefVervolgbeleidVoorBevolkingsonderzoekg = verslag.getVerslagContent().getColoscopieMedischeObservatie()
				.getDefinitiefVervolgbeleidVoorBevolkingsonderzoekg();
			if (definitiefVervolgbeleidVoorBevolkingsonderzoekg != null)
			{
				DSValue periodeVervolgSurveillancescopie = definitiefVervolgbeleidVoorBevolkingsonderzoekg.getPeriodeVervolgSurveillancescopie();
				if (periodeVervolgSurveillancescopie != null)
				{
					String code = periodeVervolgSurveillancescopie.getCode();
					switch (code)
					{
					case "12":
						uitnodigingsintervalType = ColonUitnodigingsintervalType.ENDOSCOPIEVERSLAG_SURVEILLANCE_1_JAAR;
						break;
					case "14":
						uitnodigingsintervalType = ColonUitnodigingsintervalType.ENDOSCOPIEVERSLAG_SURVEILLANCE_3_JAAR;
						break;
					case "15":
						uitnodigingsintervalType = ColonUitnodigingsintervalType.ENDOSCOPIEVERSLAG_SURVEILLANCE_5_JAAR;
						break;
					}
				}
			}
		}
		else
		{
			uitnodigingsintervalType = vervolgbeleid.getUitnodigingsinterval();
		}
		dossierBaseService.setDatumVolgendeUitnodiging(dossier, uitnodigingsintervalType);
		hibernateService.saveOrUpdate(dossier);
	}

	private MdlVervolgbeleid getActueelsteVervolgbeleid(ColonScreeningRonde screeningRonde)
	{
		MdlVerslag actueelsteVerslag = verslagDao.getActueelsteMdlVerslag(screeningRonde);
		return actueelsteVerslag != null ? getVervolgbeleid(actueelsteVerslag) : null;
	}

	private MdlVervolgbeleid getVervolgbeleid(MdlVerslag mdlVerslag)
	{
		MdlVerslagContent content = mdlVerslag.getVerslagContent();
		MdlVervolgbeleid vervolgbeleid = null;
		if (content != null && content.getColoscopieMedischeObservatie() != null)
		{
			MdlDefinitiefVervolgbeleidVoorBevolkingsonderzoekg defVervolgbeleid = content.getColoscopieMedischeObservatie().getDefinitiefVervolgbeleidVoorBevolkingsonderzoekg();

			if (defVervolgbeleid != null)
			{
				DSValue definitiefVervolgbeleid = defVervolgbeleid.getDefinitiefVervolgbeleidVoorBevolkingsonderzoek();
				if (definitiefVervolgbeleid != null)
				{
					for (MdlVervolgbeleid beleid : MdlVervolgbeleid.values())
					{
						if (beleid.getCodeSystem().equals(definitiefVervolgbeleid.getCodeSystem()) && beleid.getCode().equals(definitiefVervolgbeleid.getCode()))
						{
							vervolgbeleid = beleid;
							break;
						}
					}
				}
			}
		}
		return vervolgbeleid;
	}

	private boolean complicatieUnkown(Client client, MdlVerslag mdlVerslag, Date complicatieDatum, ComplicatieErnst complicatieErnst, ComplicatieMoment complicatieMoment,
		ComplicatieSoort complicatieSoort)
	{
		boolean complicatieUnkown = false;
		if (complicatieDatum != null && complicatieErnst != null && complicatieMoment != null && complicatieSoort != null)
		{
			complicatieUnkown = true;
			for (Complicatie complicatie : client.getComplicaties())
			{
				if (complicatieDatum.equals(complicatie.getDatum()) && complicatieErnst.equals(complicatie.getErnst()) && complicatieMoment.equals(complicatie.getMoment())
					&& complicatieSoort.equals(complicatie.getSoort()))
				{
					boolean changed = false;
					if (complicatie.getMdlverslag() == null)
					{
						complicatie.setMdlverslag(mdlVerslag);
						changed = true;
					}
					if (!complicatie.isActief())
					{
						complicatie.setActief(true);
						changed = true;
					}
					if (changed)
					{
						hibernateService.saveOrUpdate(complicatie);
					}
					complicatieUnkown = false;
					break;
				}
			}
		}
		return complicatieUnkown;
	}

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public void onAfterVerwerkVerslagContent(MdlVerslag verslag)
	{

		MdlVerslagContent content = verslag.getVerslagContent();
		DateTime aanvangVerrichting = new DateTime(content.getVerrichting().getAanvangVerrichting());
		if (content.getColoscopieMedischeObservatie() != null)
		{
			MdlDefinitiefVervolgbeleidVoorBevolkingsonderzoekg defVervolgbeleid = content.getColoscopieMedischeObservatie().getDefinitiefVervolgbeleidVoorBevolkingsonderzoekg();

			if (defVervolgbeleid != null)
			{
				fixSurveillanceColoscopie(defVervolgbeleid, aanvangVerrichting);
				fixDefinitiefVervolgbeleid(content.getColoscopieMedischeObservatie());
			}
			fixAfbrekenColoscopie(content.getColoscopieMedischeObservatie());
			bepaalEnSetUitnodigingeinterval(verslag, verslag.getScreeningRonde().getDossier());
		}
		verslag.setDatumOnderzoek(aanvangVerrichting.toDate());

		if (verslag.getDatumOnderzoek() == null || complicatieService.magComplicatieVastleggen(verslag.getDatumOnderzoek()))
		{
			koppelMdlVerslagAanComplicatie(verslag);
			maakClientComplicatieUitMdlVerslag(verslag, content);
		}
		else if (content.getVerrichting() != null && CollectionUtils.isNotEmpty(content.getVerrichting().getIncidentcomplicatie()))
		{
			List<MdlIncidentcomplicatie> incidentcomplicaties = content.getVerrichting().getIncidentcomplicatie();
			if (incidentcomplicaties.get(0).getId() != null)
			{
				hibernateService.deleteAll(incidentcomplicaties);
			}
			incidentcomplicaties.clear();
			hibernateService.saveOrUpdate(content.getVerrichting());
		}
		converteerVolledigheidWegnameMateriaal(verslag);
	}

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public void onAfterVerwerkVerslagContent(PaVerslag verslag)
	{

		PaVerslagContent content = verslag.getVerslagContent();
		if (content.getVerrichting() != null)
		{
			verslag.setDatumOnderzoek(content.getVerrichting().getAanvangVerrichting());
			if (verslag.getDatumOnderzoek() == null)
			{
				verslag.setDatumOnderzoek(content.getVerrichting().getEindeVerrichting());
			}
		}
		DSValue consultrevisieMateriaalAangevraagd = verslag.getVerslagContent().getPathologieMedischeObservatie().getConsultrevisieMateriaalAangevraagd();

		if (consultrevisieMateriaalAangevraagd != null)
		{
			for (PaPathologieProtocolColonbioptperPoliep paPathologieProtocolColonbioptperPoliep : verslag.getVerslagContent().getPathologieProtocolColonbioptperPoliep())
			{
				paPathologieProtocolColonbioptperPoliep.setConsultMateriaalAangevraagd(consultrevisieMateriaalAangevraagd);
			}
			verslag.getVerslagContent().getPathologieMedischeObservatie().setConsultrevisieMateriaalAangevraagd(null);
		}
	}

	private void converteerVolledigheidWegnameMateriaal(MdlVerslag mdlVerslag)
	{
		if (mdlVerslag.getVerslagContent().getVersie().ordinal() < VerslagGeneratie.V4.ordinal())
		{
			DSValue inTotoCompleet = verslagDao.getDsValue("255619001", "2.16.840.1.113883.6.96", "vs_verwijdering_compleet");
			DSValue piecemealCompleet = verslagDao.getDsValue("2", "2.16.840.1.113883.2.4.3.36.77.5.35", "vs_verwijdering_compleet");
			DSValue incompleet = verslagDao.getDsValue("255599008", "2.16.840.1.113883.6.96", "vs_verwijdering_compleet");

			DSValue inToto = verslagDao.getDsValue("255619001", "2.16.840.1.113883.6.96", "vs_method_of_excision");
			DSValue piecemeal = verslagDao.getDsValue("2", "2.16.840.1.113883.2.4.3.36.77.5.35", "vs_method_of_excision");
			DSValue radicaal = verslagDao.getDsValue("255612005", "2.16.840.1.113883.6.96", "vs_extent");
			DSValue irradicaal = verslagDao.getDsValue("255599008", "2.16.840.1.113883.6.96", "vs_extent");
			for (MdlLaesiecoloscopiecentrum mdlLaesiecoloscopiecentrum : mdlVerslag.getVerslagContent().getLaesiecoloscopiecentrum())
			{
				MdlPoliep poliep = mdlLaesiecoloscopiecentrum.getPoliep();
				DSValue volledigheidWegnameMateriaal = poliep.getVolledigheidWegnameMateriaal();
				if (volledigheidWegnameMateriaal != null)
				{
					if (volledigheidWegnameMateriaal.equals(inTotoCompleet))
					{
						poliep.setMethodeVanVerwijderen(inToto);
						poliep.setResultaatVerwijdering(radicaal);
					}
					else if (volledigheidWegnameMateriaal.equals(piecemealCompleet))
					{
						poliep.setMethodeVanVerwijderen(piecemeal);
						poliep.setResultaatVerwijdering(radicaal);
					}
					else if (volledigheidWegnameMateriaal.equals(incompleet))
					{
						poliep.setResultaatVerwijdering(irradicaal);
					}
					poliep.setVolledigheidWegnameMateriaal(null);
				}
			}
		}
	}

	private void fixAfbrekenColoscopie(MdlColoscopieMedischeObservatie coloscopieMedischeObservatie)
	{
		if (coloscopieMedischeObservatie.getRedenAfbrekingColoscopie() != null)
		{
			List<DSValue> oudeWaarden = new ArrayList<>();
			for (DSValue afbrekenColoscopie : coloscopieMedischeObservatie.getRedenAfbrekingColoscopie())
			{
				if (afbrekenColoscopie.getCode().equals("6") || afbrekenColoscopie.getCode().equals("7"))
				{
					oudeWaarden.add(afbrekenColoscopie);
				}
			}
			if (oudeWaarden.size() > 0)
			{
				coloscopieMedischeObservatie.getRedenAfbrekingColoscopie().removeAll(oudeWaarden);
				coloscopieMedischeObservatie.getRedenAfbrekingColoscopie().add(verslagDao.getDsValue("12", "2.16.840.1.113883.2.4.3.36.77.5.37", "vs_afbreken_coloscopie"));
			}

		}
		DSValue redenCoecumNietBereikt = coloscopieMedischeObservatie.getRedenCoecumNietBereikt();
		if (redenCoecumNietBereikt != null)
		{
			if (redenCoecumNietBereikt.getCode().equals("6") || redenCoecumNietBereikt.getCode().equals("7"))
			{
				coloscopieMedischeObservatie.setRedenCoecumNietBereikt(verslagDao.getDsValue("12", "2.16.840.1.113883.2.4.3.36.77.5.37", "vs_afbreken_coloscopie"));
			}
		}
	}

	private void fixDefinitiefVervolgbeleid(MdlColoscopieMedischeObservatie coloscopieMedischeObservatie)
	{
		MdlDefinitiefVervolgbeleidVoorBevolkingsonderzoekg definitiefVervolgbeleidgroep = coloscopieMedischeObservatie.getDefinitiefVervolgbeleidVoorBevolkingsonderzoekg();
		DSValue vervolgbeleidNavAfbrekingColoscopie = coloscopieMedischeObservatie.getVervolgbeleidNavAfbrekingColoscopie();
		if (vervolgbeleidNavAfbrekingColoscopie != null)
		{
			if (vervolgbeleidNavAfbrekingColoscopie.getCode().equals("2"))
			{

				definitiefVervolgbeleidgroep.setDefinitiefVervolgbeleidVoorBevolkingsonderzoek(verslagDao.getDsValue("418714002", "2.16.840.1.113883.6.96", "vs_vervolgbeleid"));
			}
			else if (definitiefVervolgbeleidgroep.getDefinitiefVervolgbeleidVoorBevolkingsonderzoek() != null && (vervolgbeleidNavAfbrekingColoscopie.getCode().equals("1")
				&& definitiefVervolgbeleidgroep.getDefinitiefVervolgbeleidVoorBevolkingsonderzoek().getCode().equals("183851006")
				|| definitiefVervolgbeleidgroep.getDefinitiefVervolgbeleidVoorBevolkingsonderzoek().getCode().equals("73761001")))
			{

				definitiefVervolgbeleidgroep
					.setDefinitiefVervolgbeleidVoorBevolkingsonderzoek(verslagDao.getDsValue("73761001:260870009=64695001", "2.16.840.1.113883.6.96", "vs_vervolgbeleid"));
			}
		}
	}

	private void fixSurveillanceColoscopie(MdlDefinitiefVervolgbeleidVoorBevolkingsonderzoekg defVervolgbeleid, DateTime aanvangVerrichting)
	{
		MdlVerslagContent content = defVervolgbeleid.getColoscopieMedischeObservatie().getVerslagContent();

		Quantity surveillancecoloscopieCda = defVervolgbeleid.getPeriodeVervolgSurveillancecoloscopieCda();
		String value = null;
		String unit = null;
		if (surveillancecoloscopieCda != null)
		{
			value = surveillancecoloscopieCda.getValue();
			unit = surveillancecoloscopieCda.getUnit();
		}

		if (value != null && StringUtils.isBlank(unit) && aanvangVerrichting != null && content.getVersie().ordinal() < VerslagGeneratie.V3.ordinal())
		{
			try
			{
				MutableDateTime periodeDatum = null;
				if (value.length() == 6)
				{
					periodeDatum = new MutableDateTime(new SimpleDateFormat("yyyyMM").parse(value));
				}
				else if (value.length() == 4)
				{
					periodeDatum = new MutableDateTime(new SimpleDateFormat("yyyy").parse(value));
					periodeDatum.setMonthOfYear(aanvangVerrichting.getMonthOfYear());
				}
				if (periodeDatum != null)
				{
					periodeDatum.setDayOfMonth(1);
					periodeDatum.setMillisOfDay(0); 
					int months = DateUtil.getMonthsBetweenDates(aanvangVerrichting.toDate(), periodeDatum.toDate());
					value = "" + months;
					unit = "maand";
					surveillancecoloscopieCda.setUnit(unit);
					surveillancecoloscopieCda.setValue(value);
				}
			}
			catch (ParseException e)
			{
				LOG.error("Fout bij vertalen surveillancecoloscopie naar waarde en unit " + value);
			}
		}

		boolean cdaBericht = content.getVerslag().getOntvangenBericht() != null;
		if (defVervolgbeleid.getPeriodeVervolgSurveillancescopie() == null)
		{

			if (StringUtils.isNotBlank(unit) && StringUtils.isNotBlank(value) && cdaBericht)
			{
				DSValue dsValue = null;
				try
				{
					int codeValue = Double.valueOf(value).intValue();
					String codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.226";
					String valueSetName = "vs_periode_vervolg";
					String code2Jaar = "13";
					String code3Jaar = "14";
					String code5Jaar = "15";
					if (unit.equals("maand") || unit.equals("mo"))
					{
						if (codeValue > 0 && codeValue <= 12)
						{
							dsValue = verslagDao.getDsValue(codeValue + "", codeSystem, valueSetName);
						}
						else if (codeValue == 24)
						{
							dsValue = verslagDao.getDsValue(code2Jaar, codeSystem, valueSetName);
						}
						else if (codeValue == 36)
						{
							dsValue = verslagDao.getDsValue(code3Jaar, codeSystem, valueSetName);
						}
						else if (codeValue == 60)
						{
							dsValue = verslagDao.getDsValue(code5Jaar, codeSystem, valueSetName);
						}
					}
					else if (unit.equals("jaar"))
					{
						if (codeValue == 2)
						{
							dsValue = verslagDao.getDsValue(code2Jaar, codeSystem, valueSetName);
						}
						else if (codeValue == 3)
						{
							dsValue = verslagDao.getDsValue(code3Jaar, codeSystem, valueSetName);
						}
						else if (codeValue == 5)
						{
							dsValue = verslagDao.getDsValue(code5Jaar, codeSystem, valueSetName);
						}
					}

					if (dsValue != null)
					{
						defVervolgbeleid.setPeriodeVervolgSurveillancescopie(dsValue);
					}
				}
				catch (NumberFormatException e)
				{
					LOG.error("Fout bij vertalen surveillancecoloscopie van cda waarde naar intvalue " + value);
				}
			}
			else
			{

				if (surveillancecoloscopieCda != null)
				{
					surveillancecoloscopieCda.setUnit(null);
					surveillancecoloscopieCda.setValue(null);
				}
			}
		}
		else
		{

			DSValue periodeVervolgSurveillancecoloscopie = defVervolgbeleid.getPeriodeVervolgSurveillancescopie();
			unit = "maand";
			String code = periodeVervolgSurveillancecoloscopie.getCode();
			switch (code)
			{
			case "1":
			case "2":
			case "3":
			case "4":
			case "5":
			case "6":
			case "7":
			case "8":
			case "9":
			case "10":
			case "12":
				value = Double.valueOf(code).intValue() + "";
				break;
			case "13":
				value = "24"; 
				break;
			case "14":
				value = "36"; 
				break;
			case "15":
				value = "60"; 
				break;
			default:
				unit = null;
				value = null;
			}

			if (surveillancecoloscopieCda == null)
			{
				surveillancecoloscopieCda = new Quantity();
				defVervolgbeleid.setPeriodeVervolgSurveillancecoloscopieCda(surveillancecoloscopieCda);
			}
			if (StringUtils.isNotBlank(unit) && StringUtils.isNotBlank(value))
			{
				surveillancecoloscopieCda.setUnit(unit);
				surveillancecoloscopieCda.setValue(value);
			}
		}
	}

	private void koppelMdlVerslagAanComplicatie(MdlVerslag verslag)
	{
		Client client = verslag.getScreeningRonde().getDossier().getClient();
		List<Complicatie> complicaties = complicatieService.geefAlleNietGekoppeldeComplicaties(client, verslag.getDatumOnderzoek());
		for (Complicatie complicatie : complicaties)
		{
			complicatie.setMoment(complicatieService.getCorrecteComplicatieMoment(complicatie.getDatum(), verslag));
			complicatie.setMdlverslag(verslag);
			hibernateService.saveOrUpdate(complicatie);
		}
		hibernateService.saveOrUpdate(client);
	}

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public void ontkoppelOfVerwijderComplicaties(MdlVerslag mdlVerslag)
	{
		Client client = mdlVerslag.getScreeningRonde().getDossier().getClient();
		List<Complicatie> complToDelete = new ArrayList<>();
		for (Complicatie complicatie : client.getComplicaties())
		{
			if (mdlVerslag.equals(complicatie.getMdlverslag()))
			{
				if (!Boolean.FALSE.equals(complicatie.getHandmatig()))
				{
					complicatie.setMdlverslag(null);
					hibernateService.saveOrUpdate(complicatie);
				}
				else
				{
					complToDelete.add(complicatie);
				}
			}
		}
		for (Complicatie complicatie : complToDelete)
		{
			client.getComplicaties().remove(complicatie);
			hibernateService.delete(complicatie);
		}
		hibernateService.saveOrUpdate(client);
	}

	private void maakClientComplicatieUitMdlVerslag(MdlVerslag mdlVerslag, MdlVerslagContent verslagContent)
	{
		InstellingGebruiker instellingGebruiker = mdlVerslag.getInvoerder();
		if (instellingGebruiker == null && mdlVerslag.getUitvoerderMedewerker().getOrganisatieMedewerkers() != null)
		{
			for (InstellingGebruiker ig : mdlVerslag.getUitvoerderMedewerker().getOrganisatieMedewerkers())
			{
				if (Boolean.TRUE.equals(ig.getActief()) && Boolean.TRUE.equals(ig.getOrganisatie().getActief())
					&& ig.getOrganisatie().equals(mdlVerslag.getUitvoerderOrganisatie()))
				{
					instellingGebruiker = ig;
					break;
				}
			}
			if (instellingGebruiker == null && OrganisatieType.ZORGINSTELLING.equals(mdlVerslag.getUitvoerderOrganisatie().getOrganisatieType()))
			{
				for (InstellingGebruiker ig : mdlVerslag.getUitvoerderMedewerker().getOrganisatieMedewerkers())
				{
					if (Boolean.TRUE.equals(ig.getActief()) && Boolean.TRUE.equals(ig.getOrganisatie().getActief()) && ig.getOrganisatie().getParent() != null
						&& Boolean.TRUE.equals(ig.getOrganisatie().getParent().getActief()) && ig.getOrganisatie().getParent().equals(mdlVerslag.getUitvoerderOrganisatie()))
					{
						instellingGebruiker = ig;
						break;
					}
				}
			}
		}

		Client client = mdlVerslag.getScreeningRonde().getDossier().getClient();
		if (verslagContent != null && verslagContent.getVerrichting() != null && CollectionUtils.isNotEmpty(verslagContent.getVerrichting().getIncidentcomplicatie()))
		{
			for (MdlIncidentcomplicatie incidentcomplicatie : verslagContent.getVerrichting().getIncidentcomplicatie())
			{
				Date complicatieDatum = mdlVerslag.getDatumOnderzoek();
				ComplicatieErnst complicatieErnst = ComplicatieErnst.getValue(incidentcomplicatie.getErnstIncidentcomplicatie());
				ComplicatieMoment complicatieMoment = ComplicatieMoment.BINNEN_24_UUR;
				ComplicatieSoort complicatieSoort = ComplicatieSoort.getValue(incidentcomplicatie.getTypeIncidentcomplicatie());

				if (complicatieUnkown(client, mdlVerslag, complicatieDatum, complicatieErnst, complicatieMoment, complicatieSoort))
				{
					Complicatie complicatie = new Complicatie();
					complicatie.setActief(true);
					complicatie.setClient(client);
					complicatie.setHandmatig(false);
					complicatie.setInstellingGebruiker(instellingGebruiker);
					complicatie.setDatum(complicatieDatum);
					complicatie.setErnst(complicatieErnst);
					complicatie.setMoment(complicatieMoment);
					complicatie.setMdlverslag(mdlVerslag);

					complicatie.setSoort(complicatieSoort);
					client.getComplicaties().add(complicatie);

					hibernateService.saveOrUpdate(complicatie);
				}
			}
			hibernateService.saveOrUpdate(client);
		}
	}

	@Override
	public ColonScreeningRonde getValideScreeningsRonde(Client client, Verslag oudeVersieVerslag, Date onderzoeksdatum)
	{
		ColonDossier dossier = client.getColonDossier();
		ColonScreeningRonde rondeVoorVerslag = null;
		if (oudeVersieVerslag instanceof ColonVerslag)
		{

			rondeVoorVerslag = ((ColonVerslag<?>) oudeVersieVerslag).getScreeningRonde();
		}
		if (rondeVoorVerslag == null)
		{
			List<ColonScreeningRonde> screeningRondes = new ArrayList<>(dossier.getScreeningRondes());
			Collections.sort(screeningRondes, new PropertyComparator<ColonScreeningRonde>("creatieDatum", false, false));

			boolean heeftOngunstigeUitslagOuderDanOnderzoeksdatum = onderzoeksdatum == null
				|| screeningRondes.stream().anyMatch(r -> isOngunstigeUitslagVerwerktVoorOnderzoeksdatum(r, onderzoeksdatum));

			if (heeftOngunstigeUitslagOuderDanOnderzoeksdatum)
			{
				rondeVoorVerslag = screeningRondes.stream().filter(r -> ColonScreeningRondeUtil.getEersteOngunstigeTest(r) != null || r.getOpenUitnodiging() != null).findFirst()
					.orElse(null);
			}
		}
		return rondeVoorVerslag;
	}

	private boolean isOngunstigeUitslagVerwerktVoorOnderzoeksdatum(ColonScreeningRonde ronde, Date onderzoeksdatum)
	{
		IFOBTTest eersteOngunstigeTest = ColonScreeningRondeUtil.getEersteOngunstigeTest(ronde);
		return eersteOngunstigeTest != null && eersteOngunstigeTest.getVerwerkingsDatum().compareTo(onderzoeksdatum) < 0;
	}

	@Override
	public void valideerVerslagVoorAfronden(PaVerslag verslag, InstellingGebruiker instellingGebruiker)
	{
		PaVerslagContent paVerslagContent = verslag.getVerslagContent();
		MdlVerslag mdlVerslag = null;
		if (paVerslagContent.getPathologieMedischeObservatie() != null)
		{
			mdlVerslag = verslagDao.getMdlVerslagMetTNummer(paVerslagContent);
		}
		if (mdlVerslag != null)
		{
			Instelling paLab = instellingGebruiker.getOrganisatie();
			Instelling mdlOrganisatie = mdlVerslag.getUitvoerderOrganisatie();
			boolean paLabGekoppeld = false;
			if (OrganisatieType.PA_LABORATORIUM.equals(paLab.getOrganisatieType()))
			{
				PaLaboratorium paLaboratorium = hibernateService.load(PaLaboratorium.class, paLab.getId());
				for (Instelling locatie : paLaboratorium.getColoscopielocaties())
				{
					switch (mdlOrganisatie.getOrganisatieType())
					{
					case COLOSCOPIELOCATIE:
						if (locatie.equals(mdlOrganisatie))
						{
							paLabGekoppeld = true;
						}
						break;
					case ZORGINSTELLING:
						if (locatie.getParent() != null && locatie.getParent().equals(mdlOrganisatie))
						{
							paLabGekoppeld = true;
						}
						break;
					default:
						break;
					}
				}
			}
			else
			{
				paLabGekoppeld = true;
			}
			if (!paLabGekoppeld)
			{
				throw new IllegalStateException("error.pa.lab.niet.gekoppeld");
			}
		}
		if (getValideScreeningsRonde(verslag.getScreeningRonde().getDossier().getClient(), null, paVerslagContent.getVerrichting().getAanvangVerrichting()) == null)
		{
			throw new IllegalStateException("error.aanvang.verrichting.voor.eerste.ongunstige.uitslag");
		}

	}

	@Override
	public void valideerVerslagVoorAfronden(MdlVerslag verslag, Set<Antwoord<?>> antwoorden, InstellingGebruiker instellingGebruiker)
	{
		MdlVerslagContent mdlVerslagContent = verslag.getVerslagContent();

		List<MdlLaesiecoloscopiecentrum> laesies = mdlVerslagContent.getLaesiecoloscopiecentrum();

		int aantalNietIngezonden = 0;
		int totaalAantalGedetecteerde = 0;
		MdlColoscopieMedischeObservatie coloscopieMedischeObservatie = mdlVerslagContent.getColoscopieMedischeObservatie();
		if (coloscopieMedischeObservatie != null)
		{
			aantalNietIngezonden = getIntegerValue(coloscopieMedischeObservatie.getAantalVerwijderdeLaesiesNietIngezondenVoorPaEnG());
			totaalAantalGedetecteerde = getIntegerValue(coloscopieMedischeObservatie.getTotaalAantalGedetecteerdeLaesies());
		}

		int aantalLaesies = laesies.size();
		boolean leasieJaNeeFound = false;
		boolean datumVerrichtingFound = false;
		boolean incidentComplicatieFound = false;
		boolean incidentComplicatieGeselecteerd = false;

		for (Antwoord<?> antwoord : antwoorden)
		{
			VraagDefinitie<?> vraagDefinitie = antwoord.getVraagInstantie().getVraagDefinitie();
			if (vraagDefinitie instanceof IdentifierElement)
			{
				IdentifierElement identifierElement = (IdentifierElement) vraagDefinitie;
				if (identifierElement.getIdentifier() != null)
				{
					switch (identifierElement.getIdentifier())
					{
					case Constants.VRAAG_LAESIE_JA_NEE:

						if (antwoord instanceof BooleanAntwoord)
						{
							BooleanAntwoord booleanAntwoord = (BooleanAntwoord) antwoord;
							Boolean value = booleanAntwoord.getValue();
							if (!Boolean.TRUE.equals(value))
							{
								aantalLaesies = 0;
							}
						}
						leasieJaNeeFound = true;
						break;
					case Constants.VRAAG_DATUM_VERRICHTING:
						if (antwoord instanceof DateAntwoord)
						{
							DateAntwoord dateAntwoord = (DateAntwoord) antwoord;
							Date value = dateAntwoord.getValue();
							if (value != null)
							{
								mdlVerslagContent.getVerrichting().setAanvangVerrichting(value);
							}
						}
						datumVerrichtingFound = true;
						break;
					case Constants.VRAAG_INCIDENT_COMPLICATIE_JA_NEE:
						if (antwoord instanceof BooleanAntwoord)
						{
							BooleanAntwoord booleanAntwoord = (BooleanAntwoord) antwoord;
							Boolean value = booleanAntwoord.getValue();
							incidentComplicatieGeselecteerd = Boolean.TRUE.equals(value);
						}
						incidentComplicatieFound = true;
						break;
					default:
						break;
					}
				}
			}
			if (leasieJaNeeFound && datumVerrichtingFound && incidentComplicatieFound)
			{
				break;
			}
		}

		valideerAantalLeasies(aantalNietIngezonden, totaalAantalGedetecteerde, aantalLaesies);

		List<Integer> usedPotjeNummers = valideerPotjesNummers(mdlVerslagContent);

		valideerTNummerInvoer(mdlVerslagContent, usedPotjeNummers);

		valideerPALabGekoppeldAanCLBijOvereenkomstigTNummer(mdlVerslagContent, instellingGebruiker);

		valideerOnderzoeksdatumInvoer(mdlVerslagContent, incidentComplicatieGeselecteerd);

		valideerSurveillanceInvoer(mdlVerslagContent);
	}

	private void valideerAantalLeasies(int aantalNietIngezonden, int totaalAantalGedetecteerde, int aantalLaesies)
	{
		if (aantalLaesies + aantalNietIngezonden != totaalAantalGedetecteerde)
		{
			throw new IllegalStateException("error.totaal.laesies.ongelijk.gedetecteerde.plus.niet.ingezonden");
		}
	}

	private List<Integer> valideerPotjesNummers(MdlVerslagContent mdlVerslagContent)
	{

		List<Integer> usedPotjeNummers = new ArrayList<>();
		for (MdlLaesiecoloscopiecentrum leasie : mdlVerslagContent.getLaesiecoloscopiecentrum())
		{
			String nummerPotjeMonster = leasie.getNummerPotjeMonster();
			if (StringUtils.isNotBlank(nummerPotjeMonster))
			{
				Integer uniformPotjenummer = null;
				if (StringUtils.isNumeric(nummerPotjeMonster))
				{
					uniformPotjenummer = Integer.valueOf(nummerPotjeMonster);
				}
				else
				{
					try
					{
						uniformPotjenummer = RomanNumeral.toInteger(nummerPotjeMonster);
					}
					catch (IllegalArgumentException e)
					{
						throw new IllegalStateException("error.mdl.potjenummers.geen.nummer");
					}
				}
				if (usedPotjeNummers.contains(uniformPotjenummer))
				{
					throw new IllegalStateException("error.mdl.potjenummers.niet.uniek");
				}
				else
				{
					usedPotjeNummers.add(uniformPotjenummer);
				}
			}
		}
		return usedPotjeNummers;
	}

	private void valideerTNummerInvoer(MdlVerslagContent mdlVerslagContent, List<Integer> usedPotjeNummers)
	{

		if (usedPotjeNummers.size() > 0)
		{
			boolean hasTnummer = false;
			if (mdlVerslagContent.getColoscopieMedischeObservatie() != null)
			{
				for (MdlTnummerPathologieVerslag tnummer : mdlVerslagContent.getColoscopieMedischeObservatie().getTnummerPathologieVerslag())
				{
					if (StringUtils.isNotEmpty(tnummer.getTnummerPathologieVerslag()))
					{
						hasTnummer = true;
						break;
					}
				}
			}
			if (!hasTnummer)
			{
				throw new IllegalStateException("error.mdl.tnummer.verplicht.bij.monster");
			}
		}
	}

	private void valideerPALabGekoppeldAanCLBijOvereenkomstigTNummer(MdlVerslagContent mdlVerslagContent, InstellingGebruiker instellingGebruiker)
	{

		List<PaVerslag> paVerslagen = verslagDao.getPaVerslagMetTNummer(mdlVerslagContent);
		for (PaVerslag paVerslag : paVerslagen)
		{
			Instelling mdlOrganisatie = instellingGebruiker.getOrganisatie();
			Instelling paLab = paVerslag.getUitvoerderOrganisatie();
			boolean paLabGekoppeld = false;
			if (OrganisatieType.PA_LABORATORIUM.equals(paLab.getOrganisatieType()))
			{
				PaLaboratorium paLaboratorium = hibernateService.load(PaLaboratorium.class, paLab.getId());
				for (Instelling locatie : paLaboratorium.getColoscopielocaties())
				{
					switch (mdlOrganisatie.getOrganisatieType())
					{
					case COLOSCOPIELOCATIE:
						if (locatie.equals(mdlOrganisatie))
						{
							paLabGekoppeld = true;
						}
						break;
					case ZORGINSTELLING:
						if (locatie.getParent() != null && locatie.getParent().equals(mdlOrganisatie))
						{
							paLabGekoppeld = true;
						}
						break;
					default:
						break;
					}
				}
			}
			else
			{
				paLabGekoppeld = true;
			}
			if (!paLabGekoppeld)
			{
				throw new IllegalStateException("error.mdl.lab.niet.gekoppeld");
			}
		}
	}

	private void valideerOnderzoeksdatumInvoer(MdlVerslagContent mdlVerslagContent, boolean incidentComplicatieGeselecteerd)
	{

		Date aanvangVerrichting = mdlVerslagContent.getVerrichting().getAanvangVerrichting();
		boolean hasMdlVerslagWithOnderzoekDatum = verslagDao.getMdlVerslagenWithOnderzoekDatum(mdlVerslagContent.getVerslag(), aanvangVerrichting) != 0;
		if (hasMdlVerslagWithOnderzoekDatum)
		{
			throw new IllegalStateException("error.mdl.onderzoekdatum.al.gebruikt");
		}
		if (aanvangVerrichting != null && !complicatieService.magComplicatieVastleggen(aanvangVerrichting)
			&& CollectionUtils.isNotEmpty(mdlVerslagContent.getVerrichting().getIncidentcomplicatie()) && incidentComplicatieGeselecteerd)
		{
			throw new IllegalStateException("error.mdl.complicaties.niet.meer.vastleggen");
		}
		if (getValideScreeningsRonde(mdlVerslagContent.getVerslag().getScreeningRonde().getDossier().getClient(), null, aanvangVerrichting) == null)
		{
			throw new IllegalStateException("error.aanvang.verrichting.voor.eerste.ongunstige.uitslag");
		}
	}

	private void valideerSurveillanceInvoer(MdlVerslagContent mdlVerslagContent)
	{

		MdlVervolgbeleid vervolgbeleid = getVervolgbeleid(mdlVerslagContent.getVerslag());
		MdlColoscopieMedischeObservatie coloscopieMedischeObservatie = mdlVerslagContent.getColoscopieMedischeObservatie();
		if (vervolgbeleid != null && coloscopieMedischeObservatie != null)
		{
			DSValue periodeVervolgSurveillancescopie = coloscopieMedischeObservatie.getDefinitiefVervolgbeleidVoorBevolkingsonderzoekg().getPeriodeVervolgSurveillancescopie();
			String code = null;
			if (periodeVervolgSurveillancescopie != null)
			{
				code = periodeVervolgSurveillancescopie.getCode();
			}
			if (vervolgbeleid == MdlVervolgbeleid.SURVEILLANCE)
			{
				if (!SURVAILLANCE_CODES.contains(code))
				{
					throw new IllegalStateException("error.mdl.surveillance.alleen.1.3.5.jaar");
				}
			}
			else if (vervolgbeleid == MdlVervolgbeleid.POLIEPECTOMIE || vervolgbeleid == MdlVervolgbeleid.COLONOSCOPY || vervolgbeleid == MdlVervolgbeleid.COLONOSCOPY_NEW)
			{
				if (SURVAILLANCE_CODES.contains(code))
				{
					throw new IllegalStateException("error.mdl.scopieen.alleen.maanden");
				}
			}
		}
	}

	private static int getIntegerValue(Quantity quantity)
	{
		int aantalNietIngezonden = 0;
		if (quantity != null && NumberUtils.isNumber(quantity.getValue()))
		{
			aantalNietIngezonden = (int) Double.parseDouble(quantity.getValue());
		}
		return aantalNietIngezonden;
	}
}
