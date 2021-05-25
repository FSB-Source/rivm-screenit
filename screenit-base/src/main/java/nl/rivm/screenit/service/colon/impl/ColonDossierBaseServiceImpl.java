
package nl.rivm.screenit.service.colon.impl;

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

import java.time.LocalDate;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import nl.rivm.screenit.model.colon.ColonConclusie;
import nl.rivm.screenit.model.colon.ColonDossier;
import nl.rivm.screenit.model.colon.ColonIntakeAfspraak;
import nl.rivm.screenit.model.colon.ColonScreeningRonde;
import nl.rivm.screenit.model.colon.ColonUitnodigingsinterval;
import nl.rivm.screenit.model.colon.ColonVolgendeUitnodiging;
import nl.rivm.screenit.model.colon.MdlVerslag;
import nl.rivm.screenit.model.colon.enums.ColonConclusieType;
import nl.rivm.screenit.model.colon.enums.ColonUitnodigingsintervalType;
import nl.rivm.screenit.model.colon.enums.MdlVervolgbeleid;
import nl.rivm.screenit.model.colon.planning.AfspraakStatus;
import nl.rivm.screenit.model.colon.verslag.mdl.MdlDefinitiefVervolgbeleidVoorBevolkingsonderzoekg;
import nl.rivm.screenit.model.enums.IntervalEenheidAanduiding;
import nl.rivm.screenit.model.verslag.DSValue;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.colon.ColonDossierBaseService;
import nl.rivm.screenit.util.ColonScreeningRondeUtil;
import nl.rivm.screenit.util.DateUtil;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Service
@Transactional(propagation = Propagation.SUPPORTS, readOnly = true)
public class ColonDossierBaseServiceImpl implements ColonDossierBaseService
{

	@Autowired
	private HibernateService hibernateService;

	@Autowired
	private ICurrentDateSupplier dateSupplier;

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

		return berekenDatumVolgendeUitnodiging(volgendeUitnodiging.getPeildatum(), volgendeUitnodiging.getInterval());
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
		if (verslag.getVervolgbeleid() == null)
		{
			uitnodigingsintervalType = ColonUitnodigingsintervalType.ENDOSCOPIEVERSLAG_TERUG_BVO;
		}
		else if (verslag.getVervolgbeleid() == MdlVervolgbeleid.SURVEILLANCE)
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
			uitnodigingsintervalType = verslag.getVervolgbeleid().getUitnodigingsinterval();
		}
		setDatumVolgendeUitnodiging(verslag.getScreeningRonde().getDossier(), uitnodigingsintervalType);
	}

	@Override
	public LocalDate getTheoretischeDatumVolgendeUitnodiging(ColonDossier dossier, ColonUitnodigingsintervalType interval)
	{
		return berekenDatumVolgendeUitnodiging(getPeildatum(dossier, interval), getIntervalByType(interval));
	}

}
