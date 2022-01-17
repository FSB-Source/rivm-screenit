package nl.rivm.screenit.main.service.impl;

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

import java.util.Date;
import java.util.List;

import nl.rivm.screenit.PreferenceKey;
import nl.rivm.screenit.main.model.testen.TestTimeLineDossierTijdstip;
import nl.rivm.screenit.main.service.TestTimelineTimeService;
import nl.rivm.screenit.model.berichten.enums.VerslagType;
import nl.rivm.screenit.model.colon.ColonBrief;
import nl.rivm.screenit.model.colon.ColonConclusie;
import nl.rivm.screenit.model.colon.ColonDossier;
import nl.rivm.screenit.model.colon.ColonHuisartsBericht;
import nl.rivm.screenit.model.colon.ColonIntakeAfspraak;
import nl.rivm.screenit.model.colon.ColonScreeningRonde;
import nl.rivm.screenit.model.colon.ColonUitnodiging;
import nl.rivm.screenit.model.colon.ColonVerslag;
import nl.rivm.screenit.model.colon.ColonVooraankondiging;
import nl.rivm.screenit.model.colon.IFOBTTest;
import nl.rivm.screenit.service.BaseTestTimelineService;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.topicuszorg.preferencemodule.service.SimplePreferenceService;

import org.joda.time.DateTime;
import org.joda.time.Days;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Service
@Transactional(propagation = Propagation.REQUIRED)
public class TestTimelineTimeServiceImpl implements TestTimelineTimeService
{
	private static final Logger LOG = LoggerFactory.getLogger(TestTimelineTimeService.class);

	@Autowired
	private SimplePreferenceService preferenceService;

	@Autowired
	private ICurrentDateSupplier currentDateSupplier;

	@Autowired
	private BaseTestTimelineService baseTestTimelineService;

	@Override
	public boolean calculateBackwards(ColonDossier dossier, TestTimeLineDossierTijdstip tijdstip)
	{
		int dagen = aantalDagenCalculator(dossier, tijdstip);
		calculateBackwards(dossier, dagen);
		return true;
	}

	@Override
	public boolean calculateBackwards(ColonDossier dossier, int dagen)
	{
		LOG.debug("Dossier aantal dagen terug gezet: " + dagen);
		baseTestTimelineService.rekenObjectTerug(dossier, dagen);
		baseTestTimelineService.rekenObjectTerug(dossier.getColonVooraankondiging(), dagen);
		baseTestTimelineService.rekenObjectTerug(dossier.getVolgendeUitnodiging(), dagen);
		rekenAlleColonScreeningRondesTerug(dossier, dagen);
		baseTestTimelineService.rekenAllePersoonsDatumTerug(dossier.getClient().getPersoon(), dagen);
		return true;
	}

	@Override
	public Date getVooraankondigingsPeriodeDatum()
	{
		int vooraankondigingsPeriode = preferenceService.getInteger(PreferenceKey.VOORAANKONDIGINSPERIODE.name());
		return currentDateSupplier.getDateTime().plusDays(vooraankondigingsPeriode).toDate();
	}

	private int aantalDagenCalculator(ColonDossier dossier, TestTimeLineDossierTijdstip tijdstip)
	{
		int aantalDagen = 0;
		switch (tijdstip)
		{
		case DAG_UITNODIGING_VERSTUREN:
			aantalDagen = preferenceService.getInteger(PreferenceKey.VOORAANKONDIGINSPERIODE.name());
			ColonVooraankondiging vooraankondiging = dossier.getColonVooraankondiging();
			return overgeblevenDagen(vooraankondiging.getCreatieDatum(), aantalDagen);
		case DAG_NA_UITNODIGING_KOPPELEN:
			aantalDagen = preferenceService.getInteger(PreferenceKey.VOORAANKONDIGINSPERIODE.name());
			aantalDagen = aantalDagen + 3;
			vooraankondiging = dossier.getColonVooraankondiging();
			return overgeblevenDagen(vooraankondiging.getCreatieDatum(), aantalDagen);
		case ANTWOORDFORMULIER_ONTVANGEN:
		case IFOBT_TERUG_ONTVANGEN:
			aantalDagen = 3;
			return overgeblevenDagen(dossier.getLaatsteScreeningRonde().getLaatsteUitnodiging().getVerstuurdDatum(), aantalDagen);
		case EINDE_RONDE:
			aantalDagen = preferenceService.getInteger(PreferenceKey.UITNODIGINGSINTERVAL.name());
			return overgeblevenDagen(dossier.getLaatsteScreeningRonde().getCreatieDatum(), aantalDagen);
		case DAG_HERINNERING_VERSTUREN:
			aantalDagen = preferenceService.getInteger(PreferenceKey.IFOBTRAPELPERIODE.name());
			return overgeblevenDagen(getTestMetEersteStatusDatum(dossier).getStatusDatum(), aantalDagen);
		case DAG_NA_HERINNERING_VERSTUREN:
			aantalDagen = preferenceService.getInteger(PreferenceKey.IFOBTRAPELPERIODE.name());
			aantalDagen = aantalDagen + 1;
			return overgeblevenDagen(getTestMetEersteStatusDatum(dossier).getStatusDatum(), aantalDagen);
		case INTAKE_AFSPRAAK_CONCLUSIE:
			ColonScreeningRonde ronde = dossier.getLaatsteScreeningRonde();
			ColonIntakeAfspraak afspraak = ronde.getLaatsteAfspraak();
			int dagen = Days.daysBetween(currentDateSupplier.getDateTime(), new DateTime(afspraak.getStartTime())).getDays();
			dagen = dagen + 3;
			return dagen;
		case MDL_VERSLAG:
			return 30;
		default:
			return aantalDagen;
		}
	}

	private IFOBTTest getTestMetEersteStatusDatum(ColonDossier dossier)
	{
		IFOBTTest eersteTest = null;
		ColonScreeningRonde ronde = dossier.getLaatsteScreeningRonde();
		for (IFOBTTest test : ronde.getIfobtTesten())
		{
			if (!test.isHerinnering() && (eersteTest == null || test.getStatusDatum().before(eersteTest.getStatusDatum())))
			{
				eersteTest = test;
			}
		}
		return eersteTest;
	}

	public int overgeblevenDagen(Date date, int aantalDagen)
	{
		DateTime aantdagenReverse = new DateTime().minusDays(aantalDagen);
		Days dagen = Days.daysBetween(aantdagenReverse.toLocalDate(), new DateTime(date).toLocalDate());
		return dagen.getDays() > 0 ? dagen.getDays() : 0;
	}

	private void rekenAlleColonScreeningRondesTerug(ColonDossier dossier, int dagen)
	{
		List<ColonScreeningRonde> rondes = dossier.getScreeningRondes();
		for (ColonScreeningRonde ronde : rondes)
		{
			baseTestTimelineService.rekenObjectTerug(ronde, dagen);

			rekenAlleColonUitnodigingenTerug(ronde, dagen);
			rekenAlleColonBrievenTerug(ronde, dagen);
			rekenAlleColonIntakeAfsprakenTerug(ronde, dagen);
			rekenAlleColonVerslagenTerug(ronde, dagen);
			rekenAlleColonHuisartsberichtenTerug(ronde, dagen);
		}
	}

	private void rekenAlleColonHuisartsberichtenTerug(ColonScreeningRonde ronde, int dagen)
	{
		for (ColonHuisartsBericht bericht : ronde.getHuisartsBerichten())
		{

			baseTestTimelineService.rekenObjectTerug(bericht, dagen);
		}
	}

	private void rekenAlleColonVerslagenTerug(ColonScreeningRonde ronde, int dagen)
	{
		for (ColonVerslag<?> verslag : ronde.getVerslagen())
		{
			if (VerslagType.MDL.equals(verslag.getType()))
			{
				baseTestTimelineService.rekenObjectTerug(verslag, dagen);
			}
			else if (VerslagType.PA_LAB.equals(verslag.getType()))
			{
				baseTestTimelineService.rekenObjectTerug(verslag, dagen);
			}
		}
	}

	private void rekenAlleColonIntakeAfsprakenTerug(ColonScreeningRonde ronde, int dagen)
	{
		for (ColonIntakeAfspraak afspraak : ronde.getAfspraken())
		{
			ColonConclusie conclusie = afspraak.getConclusie();
			if (conclusie != null)
			{
				baseTestTimelineService.rekenObjectTerug(conclusie, dagen);
			}
			baseTestTimelineService.rekenObjectTerug(afspraak, dagen);
		}
	}

	private void rekenAlleColonBrievenTerug(ColonScreeningRonde ronde, int dagen)
	{
		for (ColonBrief brief : ronde.getBrieven())
		{
			baseTestTimelineService.rekenObjectTerug(brief, dagen);

		}
	}

	private void rekenAlleColonUitnodigingenTerug(ColonScreeningRonde ronde, int dagen)
	{
		for (ColonUitnodiging uitnodiging : ronde.getUitnodigingen())
		{
			baseTestTimelineService.rekenObjectTerug(uitnodiging, dagen);
			baseTestTimelineService.rekenObjectTerug(uitnodiging.getGekoppeldeTest(), dagen);
			baseTestTimelineService.rekenObjectTerug(uitnodiging.getGekoppeldeExtraTest(), dagen);
			baseTestTimelineService.rekenObjectTerug(uitnodiging.getAntwoordFormulier(), dagen);
		}
	}
}
