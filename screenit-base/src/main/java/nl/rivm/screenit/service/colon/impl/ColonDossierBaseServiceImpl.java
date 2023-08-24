package nl.rivm.screenit.service.colon.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
 * %%
 * Copyright (C) 2012 - 2023 Facilitaire Samenwerking Bevolkingsonderzoek
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

import java.time.LocalDate;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.DossierStatus;
import nl.rivm.screenit.model.colon.ColonBrief;
import nl.rivm.screenit.model.colon.ColonConclusie;
import nl.rivm.screenit.model.colon.ColonDossier;
import nl.rivm.screenit.model.colon.ColonIntakeAfspraak;
import nl.rivm.screenit.model.colon.ColonScreeningRonde;
import nl.rivm.screenit.model.colon.ColonUitnodiging;
import nl.rivm.screenit.model.colon.ColonUitnodigingsinterval;
import nl.rivm.screenit.model.colon.ColonVolgendeUitnodiging;
import nl.rivm.screenit.model.colon.ColonVooraankondiging;
import nl.rivm.screenit.model.colon.Complicatie;
import nl.rivm.screenit.model.colon.IFOBTTest;
import nl.rivm.screenit.model.colon.MdlVerslag;
import nl.rivm.screenit.model.colon.enums.ColonConclusieType;
import nl.rivm.screenit.model.colon.enums.ColonUitnodigingsintervalType;
import nl.rivm.screenit.model.colon.enums.MdlVervolgbeleid;
import nl.rivm.screenit.model.colon.planning.AfspraakStatus;
import nl.rivm.screenit.model.colon.verslag.mdl.MdlDefinitiefVervolgbeleidVoorBevolkingsonderzoekg;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.IntervalEenheidAanduiding;
import nl.rivm.screenit.model.project.ProjectClient;
import nl.rivm.screenit.model.project.ProjectInactiefReden;
import nl.rivm.screenit.service.BaseClientContactService;
import nl.rivm.screenit.service.BaseDossierService;
import nl.rivm.screenit.service.ClientService;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.UploadDocumentService;
import nl.rivm.screenit.service.colon.ColonDossierBaseService;
import nl.rivm.screenit.util.ColonScreeningRondeUtil;
import nl.rivm.screenit.util.DateUtil;
import nl.rivm.screenit.util.ProjectUtil;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;

import org.jetbrains.annotations.NotNull;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import static org.apache.commons.collections.CollectionUtils.isEmpty;
import static org.apache.commons.collections.CollectionUtils.isNotEmpty;

@Service
@Transactional(propagation = Propagation.SUPPORTS, readOnly = true)
public class ColonDossierBaseServiceImpl implements ColonDossierBaseService
{
	@Autowired
	private HibernateService hibernateService;

	@Autowired
	private ICurrentDateSupplier dateSupplier;

	@Autowired
	private BaseClientContactService baseClientContactService;

	@Autowired
	private ICurrentDateSupplier currentDateSupplier;

	@Autowired
	private ClientService clientService;

	@Autowired
	private BaseDossierService baseDossierService;

	@Autowired
	private UploadDocumentService uploadDocumentService;

	@Override
	public LocalDate getDatumVolgendeUitnodiging(ColonDossier dossier)
	{
		ColonVolgendeUitnodiging volgendeUitnodiging = dossier.getVolgendeUitnodiging();
		if (volgendeUitnodiging == null ||
			volgendeUitnodiging.getInterval().getAantal() == null
			|| IntervalEenheidAanduiding.GEEN.equals(volgendeUitnodiging.getInterval().getEenheid()))
		{
			return null;
		}

		if (volgendeUitnodiging.getDatumVolgendeRonde() != null)
		{
			return volgendeUitnodiging.getDatumVolgendeRonde();
		}

		Date peildatum = volgendeUitnodiging.getPeildatum();
		if (volgendeUitnodiging.getProjectPeildatum() != null)
		{
			peildatum = DateUtil.toUtilDate(volgendeUitnodiging.getProjectPeildatum());
		}
		return berekenDatumVolgendeUitnodiging(peildatum, volgendeUitnodiging.getInterval());
	}

	private LocalDate berekenDatumVolgendeUitnodiging(Date peildatumDate, ColonUitnodigingsinterval interval)
	{
		Integer aantal = interval.getAantal();

		LocalDate peildatum = DateUtil.toLocalDate(peildatumDate);
		switch (interval.getEenheid())
		{
		case DAY:
			return peildatum.plusDays(aantal);
		case MONTH:
			return peildatum.plusMonths(aantal);
		case YEAR:
			return peildatum.plusYears(aantal);
		case GEEN:
			return null;
		default:
			throw new IllegalStateException("Fout in berekenen datum volgende uitnodiging");
		}
	}

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public void setDatumVolgendeUitnodiging(ColonDossier dossier, ColonUitnodigingsintervalType type)
	{
		ColonVolgendeUitnodiging volgendeUitnodiging = dossier.getVolgendeUitnodiging();
		if (volgendeUitnodiging == null)
		{
			volgendeUitnodiging = new ColonVolgendeUitnodiging();
			volgendeUitnodiging.setDossier(dossier);
			dossier.setVolgendeUitnodiging(volgendeUitnodiging);
		}
		volgendeUitnodiging.setPeildatum(getPeildatum(dossier, type));
		volgendeUitnodiging.setProjectPeildatum(null);
		volgendeUitnodiging.setDatumVolgendeRonde(null);
		volgendeUitnodiging.setInterval(getIntervalByType(type));
		hibernateService.saveOrUpdate(volgendeUitnodiging);

	}

	private Date getPeildatum(ColonDossier dossier, ColonUitnodigingsintervalType type)
	{
		Date peildatum;
		ColonScreeningRonde laatsteScreeningRonde = dossier.getLaatsteScreeningRonde();
		switch (type.peildatumSoort())
		{
		case DATUM_START_RONDE:
			peildatum = laatsteScreeningRonde.getCreatieDatum();
			break;
		case EERSTE_ONGUNSTIGE_UITSLAG_VORIGE_RONDE:
			if (laatsteScreeningRonde.getOpenUitnodiging() == null)
			{
				peildatum = ColonScreeningRondeUtil.getEersteOngunstigeTest(laatsteScreeningRonde).getVerwerkingsDatum();
			}
			else
			{
				peildatum = laatsteScreeningRonde.getCreatieDatum();
			}
			break;
		default:
			throw new IllegalStateException("Onvoorziene case bij bepalen peil datum soort.");
		}
		if (peildatum == null)
		{
			throw new IllegalStateException("Peildatum mag niet null zijn");
		}
		return peildatum;
	}

	private ColonUitnodigingsinterval getIntervalByType(ColonUitnodigingsintervalType interval)
	{
		Map<String, Object> parameters = new HashMap<>();
		parameters.put("type", interval);
		List<ColonUitnodigingsinterval> intervalParameters = hibernateService.getByParameters(ColonUitnodigingsinterval.class, parameters);

		if (intervalParameters.isEmpty())
		{
			throw new IllegalStateException("Kan interval entiteit behorende bij " + interval + " niet vinden");
		}
		return intervalParameters.get(0);
	}

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public void updateIntervalReferentieDatums()
	{
		for (ColonUitnodigingsintervalType type : ColonUitnodigingsintervalType.values())
		{
			ColonUitnodigingsinterval interval = getIntervalByType(type);
			Integer aantal = interval.getAantal();
			LocalDate referentieDatum = null;
			LocalDate vandaag = dateSupplier.getLocalDate();
			switch (interval.getEenheid())
			{
			case DAY:
				referentieDatum = vandaag.minusDays(aantal);
				break;
			case MONTH:
				referentieDatum = vandaag.minusMonths(aantal);
				break;
			case YEAR:
				referentieDatum = vandaag.minusYears(aantal);
				break;
			case GEEN:
				break;
			default:
				throw new IllegalStateException("Fout in berekenen datum volgende uitnodiging");
			}
			interval.setBerekendeReferentieDatum(DateUtil.toUtilDate(referentieDatum));
			hibernateService.saveOrUpdate(interval);
		}
	}

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public void setVolgendeUitnodingVoorConclusie(ColonIntakeAfspraak afspraak)
	{
		ColonConclusie conclusie = afspraak.getConclusie();
		ColonUitnodigingsintervalType interval = null;
		if (conclusie != null)
		{
			interval = conclusie.getType().getUitnodigingsintervalType();
			if (interval == null && conclusie.getType() == ColonConclusieType.GEEN_VERVOLGONDERZOEK)
			{
				interval = conclusie.getGeenOnderzoekReden().getUitnodigingsintervalType();
			}
		}
		else if (AfspraakStatus.isGeannuleerd(afspraak.getStatus()))
		{
			interval = ColonUitnodigingsintervalType.GEANNULEERDE_INTAKE_AFSPRAAK;
		}
		else if (afspraak.getStatus() == AfspraakStatus.GEPLAND)
		{
			interval = ColonUitnodigingsintervalType.GEPLANDE_INTAKE_AFSPRAAK;
		}
		if (interval != null)
		{
			setDatumVolgendeUitnodiging(afspraak.getColonScreeningRonde().getDossier(), interval);
		}
	}

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public void setVolgendeUitnodigingVoorVerslag(MdlVerslag verslag)
	{
		ColonUitnodigingsintervalType uitnodigingsintervalType = null;
		var screeningRondeVanVerslag = verslag.getScreeningRonde();
		var dossier = screeningRondeVanVerslag.getDossier();
		if (dossier.getLaatsteScreeningRonde().equals(screeningRondeVanVerslag))
		{
			var vervolgbeleid = getVervolgbeleid(verslag);
			if (vervolgbeleid == null)
			{
				uitnodigingsintervalType = ColonUitnodigingsintervalType.ENDOSCOPIEVERSLAG_TERUG_BVO;
			}
			else if (vervolgbeleid == MdlVervolgbeleid.SURVEILLANCE)
			{
				uitnodigingsintervalType = getUitnodigingsintervalTypeVoorSurvaillance(verslag);
			}
			else
			{
				uitnodigingsintervalType = vervolgbeleid.getUitnodigingsinterval();
			}
			setDatumVolgendeUitnodiging(dossier, uitnodigingsintervalType);
		}
	}

	@NotNull
	private static ColonUitnodigingsintervalType getUitnodigingsintervalTypeVoorSurvaillance(MdlVerslag verslag)
	{
		var uitnodigingsintervalType = ColonUitnodigingsintervalType.ENDOSCOPIEVERSLAG_SURVEILLANCE_1_JAAR;
		var definitiefVervolgbeleidVoorBevolkingsonderzoekg = verslag.getVerslagContent().getColoscopieMedischeObservatie()
			.getDefinitiefVervolgbeleidVoorBevolkingsonderzoekg();
		if (definitiefVervolgbeleidVoorBevolkingsonderzoekg != null)
		{
			var periodeVervolgSurveillancescopie = definitiefVervolgbeleidVoorBevolkingsonderzoekg.getPeriodeVervolgSurveillancescopie();
			if (periodeVervolgSurveillancescopie != null)
			{
				var code = periodeVervolgSurveillancescopie.getCode();
				switch (code)
				{
				case "12":
					return ColonUitnodigingsintervalType.ENDOSCOPIEVERSLAG_SURVEILLANCE_1_JAAR;
				case "14":
					return ColonUitnodigingsintervalType.ENDOSCOPIEVERSLAG_SURVEILLANCE_3_JAAR;
				case "15":
					return ColonUitnodigingsintervalType.ENDOSCOPIEVERSLAG_SURVEILLANCE_5_JAAR;
				default:
				}
			}
		}
		return uitnodigingsintervalType;
	}

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public void maakDossierLeeg(ColonDossier dossier)
	{
		Client client = dossier.getClient();

		if (dossier.getLaatsteScreeningRonde() != null)
		{
			setDatumVolgendeUitnodiging(dossier, ColonUitnodigingsintervalType.VERWIJDERD_DOSSIER);
		}
		verwijderRondesVanDossier(dossier);

		baseDossierService.verwijderNietLaatsteDefinitieveAfmeldingenUitDossier(dossier);

		baseClientContactService.verwijderClientContacten(client, Bevolkingsonderzoek.COLON);

		opruimenAfspraken(client);

		opruimenComplicaties(client);

		opruimenDossier(dossier);

		hibernateService.saveOrUpdate(client);

		ProjectClient projectClient = ProjectUtil.getHuidigeProjectClient(client, currentDateSupplier.getDate(), false);
		if (projectClient != null)
		{
			clientService.projectClientInactiveren(projectClient, ProjectInactiefReden.VERWIJDERING_VAN_DOSSIER, Bevolkingsonderzoek.COLON);
		}
	}

	private void opruimenDossier(ColonDossier dossier)
	{
		dossier.setInactiveerReden(null);
		dossier.setInactiefVanaf(null);
		dossier.setInactiefTotMet(null);

		if (DossierStatus.INACTIEF.equals(dossier.getStatus()) && Boolean.TRUE.equals(dossier.getAangemeld()))
		{
			dossier.setStatus(DossierStatus.ACTIEF);
		}

		hibernateService.saveOrUpdate(dossier);
	}

	private void opruimenComplicaties(Client client)
	{
		if (!isEmpty(client.getComplicaties()))
		{
			for (Complicatie complicatie : client.getComplicaties())
			{
				hibernateService.delete(complicatie);
			}
		}
	}

	private void opruimenAfspraken(Client client)
	{
		if (isNotEmpty(client.getAfspraken()))
		{
			hibernateService.deleteAll(client.getAfspraken());
			client.setAfspraken(new ArrayList<>());
		}
	}

	private void verwijderRondesVanDossier(ColonDossier dossier)
	{
		List<ColonScreeningRonde> rondes = dossier.getScreeningRondes();
		dossier.setLaatsteScreeningRonde(null);
		dossier.setScreeningRondes(new ArrayList<>());
		for (ColonScreeningRonde ronde : rondes)
		{
			maakRondeLeeg(ronde);
		}
		verwijderVoorAankondiging(dossier);
		hibernateService.deleteAll(rondes);
	}

	private void maakRondeLeeg(ColonScreeningRonde ronde)
	{
		for (ColonBrief brief : ronde.getBrieven())
		{
			brief.setIfobtTest(null);

			hibernateService.saveOrUpdate(brief);
		}
		baseDossierService.verwijderAlleAfmeldingenUitRonde(ronde);
		verwijderFITVoorUitnodigingen(ronde.getUitnodigingen());
	}

	private void verwijderVoorAankondiging(ColonDossier dossier)
	{
		if (dossier.getColonVooraankondiging() != null)
		{
			ColonVooraankondiging vooraankondigiging = dossier.getColonVooraankondiging();
			dossier.setColonVooraankondiging(null);
			ColonBrief vooraankondigigingBrief = vooraankondigiging.getBrief();
			if (vooraankondigigingBrief != null && vooraankondigigingBrief.getScreeningRonde() != null)
			{
				vooraankondigiging.setBrief(null);
				hibernateService.delete(vooraankondigigingBrief);
			}
			hibernateService.delete(vooraankondigiging);
		}
	}

	private void verwijderFITVoorUitnodigingen(List<ColonUitnodiging> uitnodigingen)
	{
		for (ColonUitnodiging uitnodiging : uitnodigingen)
		{
			IFOBTTest test = uitnodiging.getGekoppeldeTest();
			IFOBTTest extraTest = uitnodiging.getGekoppeldeExtraTest();
			uitnodiging.setGekoppeldeTest(null);
			uitnodiging.setGekoppeldeExtraTest(null);
			hibernateService.saveOrUpdate(uitnodiging);

			if (test != null)
			{
				verwijderTestenVanScreeningRonde(test);
			}
			if (extraTest != null)
			{
				verwijderTestenVanScreeningRonde(extraTest);
			}
		}
	}

	private void verwijderTestenVanScreeningRonde(IFOBTTest test)
	{
		var verwijderBrief = test.getVerwijderbrief();
		if (verwijderBrief != null)
		{
			uploadDocumentService.delete(verwijderBrief);
		}
		ColonScreeningRonde andereScreeningRonde = test.getColonScreeningRonde();
		andereScreeningRonde.setLaatsteIFOBTTest(null);
		andereScreeningRonde.setLaatsteIFOBTTestExtra(null);
		andereScreeningRonde.getIfobtTesten().remove(test);
		hibernateService.saveOrUpdate(andereScreeningRonde);
		hibernateService.delete(test);
	}

	@Override
	public LocalDate getTheoretischeDatumVolgendeUitnodiging(ColonDossier dossier, ColonUitnodigingsintervalType interval)
	{
		return berekenDatumVolgendeUitnodiging(getPeildatum(dossier, interval), getIntervalByType(interval));
	}

	@Override
	public MdlVervolgbeleid getVervolgbeleid(MdlVerslag mdlVerslag)
	{
		var content = mdlVerslag.getVerslagContent();
		var vervolgbeleid = mdlVerslag.getVervolgbeleid();
		if (vervolgbeleid == null && content != null && content.getColoscopieMedischeObservatie() != null)
		{
			var defVervolgbeleid = content.getColoscopieMedischeObservatie().getDefinitiefVervolgbeleidVoorBevolkingsonderzoekg();

			if (defVervolgbeleid != null)
			{
				vervolgbeleid = getMdlVervolgbeleid(defVervolgbeleid);
			}
		}
		return vervolgbeleid;
	}

	private static MdlVervolgbeleid getMdlVervolgbeleid(MdlDefinitiefVervolgbeleidVoorBevolkingsonderzoekg defVervolgbeleid)
	{
		MdlVervolgbeleid vervolgbeleid = null;
		var definitiefVervolgbeleid = defVervolgbeleid.getDefinitiefVervolgbeleidVoorBevolkingsonderzoek();
		if (definitiefVervolgbeleid != null)
		{
			for (var beleid : MdlVervolgbeleid.values())
			{
				if (beleid.getCodeSystem().equals(definitiefVervolgbeleid.getCodeSystem()) && beleid.getCode().equals(definitiefVervolgbeleid.getCode()))
				{
					vervolgbeleid = beleid;
					break;
				}
			}
		}
		return vervolgbeleid;
	}
}
