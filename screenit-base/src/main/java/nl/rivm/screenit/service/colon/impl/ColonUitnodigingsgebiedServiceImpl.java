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

import java.math.BigDecimal;
import java.math.RoundingMode;
import java.time.LocalDate;
import java.time.temporal.TemporalAdjusters;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import nl.rivm.screenit.PreferenceKey;
import nl.rivm.screenit.dao.colon.ColonUitnodigingsgebiedDao;
import nl.rivm.screenit.dao.colon.impl.ColonRestrictions;
import nl.rivm.screenit.model.InstellingGebruiker;
import nl.rivm.screenit.model.PostcodeGebied;
import nl.rivm.screenit.model.UitnodigingsGebied;
import nl.rivm.screenit.model.colon.CapaciteitsPercWijziging;
import nl.rivm.screenit.model.colon.ColoscopieCentrum;
import nl.rivm.screenit.model.colon.ColoscopieCentrumColonCapaciteitVerdeling;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.LogService;
import nl.rivm.screenit.service.colon.ColonDossierBaseService;
import nl.rivm.screenit.service.colon.ColonUitnodigingService;
import nl.rivm.screenit.service.colon.ColonUitnodigingsgebiedService;
import nl.rivm.screenit.util.BigDecimalUtil;
import nl.rivm.screenit.util.PercentageUtil;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
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
@Transactional(propagation = Propagation.SUPPORTS, readOnly = true)
public class ColonUitnodigingsgebiedServiceImpl implements ColonUitnodigingsgebiedService
{

	public static final Logger LOG = LoggerFactory.getLogger(ColonUitnodigingsgebiedServiceImpl.class);

	@Autowired
	private ColonUitnodigingsgebiedDao uitnodigingsGebiedDao;

	@Autowired
	private SimplePreferenceService simplePreferenceService;

	@Autowired
	private ICurrentDateSupplier currentDateSupplier;

	@Autowired
	private HibernateService hibernateService;

	@Autowired
	private LogService logService;

	@Autowired
	private ColonDossierBaseService dossierService;

	@Autowired
	private ColonUitnodigingService uitnodigingService;

	@Override
	public List<PostcodeGebied> findOverlappendePostcodeGebieden(PostcodeGebied postcode)
	{
		return uitnodigingsGebiedDao.findOverlappendePostcodeGebieden(postcode);
	}

	@Override
	public List<CapaciteitsPercWijziging> bepaalCapaciteitsWijzigingen(UitnodigingsGebied uitnodigingsGebied, Map<String, Integer> newAdherentiePercentages,
		List<ColoscopieCentrumColonCapaciteitVerdeling> verwijderdeItems)
	{
		List<CapaciteitsPercWijziging> capaciteitsWijzigingen = new ArrayList<>();

		Map<Long, BigDecimal> benodigdeIntakecapaciteiten = new HashMap<>();

		Integer percLandelijkIfobtRetour = simplePreferenceService.getInteger(PreferenceKey.PERCENTAGEIFOBTRETOUR.name());
		if (percLandelijkIfobtRetour == null)
		{
			throw new IllegalStateException("Landelijk IfobtRetourPercentage is niet gezet");
		}

		Integer percLandelijkIfobtOngunstige = simplePreferenceService.getInteger(PreferenceKey.PERCENTGAGEIFOBTONGUSTIG.name());
		if (percLandelijkIfobtOngunstige == null)
		{
			throw new IllegalStateException("Landelijk IfobtOngunstigePercentage is niet gezet");
		}

		Integer totaal = Integer.valueOf(0);

		for (Integer value : newAdherentiePercentages.values())
		{
			totaal += value;
		}
		if (!totaal.equals(Integer.valueOf(10000)) && !totaal.equals(Integer.valueOf(0)))
		{
			throw new IllegalStateException("error.totaal.adherentie.niet.100.of.0.procent");
		}

		for (ColoscopieCentrumColonCapaciteitVerdeling verdeling : uitnodigingsGebied.getVerdeling())
		{
			if (verwijderdeItems.contains(verdeling)
				|| !verdeling.getPercentageAdherentie().equals(newAdherentiePercentages.get(ColonRestrictions.getUniekIdOf(verdeling))))
			{
				ColoscopieCentrum intakelocatie = verdeling.getColoscopieCentrum();
				berekenVerschil(newAdherentiePercentages, verwijderdeItems, capaciteitsWijzigingen, benodigdeIntakecapaciteiten, percLandelijkIfobtRetour,
					percLandelijkIfobtOngunstige, intakelocatie.getCapaciteitVerdeling());
			}
		}

		if (capaciteitsWijzigingen.isEmpty() && uitnodigingsGebied.getVerdeling().size() > verwijderdeItems.size())
		{
			throw new IllegalStateException("error.adherentie.geen.wijzigingen");
		}

		Collections.sort(capaciteitsWijzigingen, new Comparator<CapaciteitsPercWijziging>()
		{
			@Override
			public int compare(CapaciteitsPercWijziging o1, CapaciteitsPercWijziging o2)
			{
				int result = o1.getUitnodigingsgebied().compareTo(o2.getUitnodigingsgebied());
				if (result == 0)
				{
					result = o1.getIntakelocatie().compareTo(o2.getIntakelocatie());
				}
				return result;
			}

		});
		return capaciteitsWijzigingen;
	}

	private BigDecimal berekenBenodigdeIntakecapaciteitVoorGebied(UitnodigingsGebied uitnodigingsGebied, Integer adherentie, Map<Long, BigDecimal> benodigdeIntakecapaciteiten,
		Integer percLandelijkIfobtRetour, Integer percLandelijkIfobtOngunstige)
	{
		Integer minimaleLeeftijd = simplePreferenceService.getInteger(PreferenceKey.MINIMALE_LEEFTIJD_COLON.name());
		if (minimaleLeeftijd == null)
		{
			throw new IllegalStateException("Minimale leeftijd colonscreening op de parameterisatie pagina is niet gezet.");
		}

		Integer maximaleLeeftijd = simplePreferenceService.getInteger(PreferenceKey.MAXIMALE_LEEFTIJD_COLON.name());
		if (maximaleLeeftijd == null)
		{
			throw new IllegalStateException("Maximale leeftijd colonscreening op de parameterisatie pagina is niet gezet");
		}

		Integer uitnodigingsInterval = simplePreferenceService.getInteger(PreferenceKey.UITNODIGINGSINTERVAL.name());
		if (uitnodigingsInterval == null)
		{
			throw new IllegalStateException("Spreidingsperiode op de parameterisatie pagina is niet gezet");
		}

		BigDecimal totaalBenodigdeIntakecapaciteit = benodigdeIntakecapaciteiten.get(uitnodigingsGebied.getId());
		if (totaalBenodigdeIntakecapaciteit == null)
		{

			LocalDate vandaag = currentDateSupplier.getLocalDate();
			LocalDate laatsteDagVanHuidigJaar = vandaag.with(TemporalAdjusters.lastDayOfYear());
			Set<Integer> alleGeboortejarenTotMetHuidigJaar = uitnodigingService.getAlleGeboortejarenTotMetHuidigJaar();

			long aantalClienten = uitnodigingsGebiedDao.countPersonenInUitnodigingsGebied(uitnodigingsGebied, minimaleLeeftijd, maximaleLeeftijd + 1, uitnodigingsInterval,
				laatsteDagVanHuidigJaar, alleGeboortejarenTotMetHuidigJaar);
			BigDecimal ifobtFactor = getIfobtFactorVoorGebied(uitnodigingsGebied, percLandelijkIfobtRetour, percLandelijkIfobtOngunstige);
			LOG.info("Uitnodigingsgebied " + uitnodigingsGebied.getNaam() + ": aantal clienten " + aantalClienten);
			totaalBenodigdeIntakecapaciteit = BigDecimal.valueOf(aantalClienten).divide(ifobtFactor, 4, RoundingMode.HALF_UP);
			benodigdeIntakecapaciteiten.put(uitnodigingsGebied.getId(), totaalBenodigdeIntakecapaciteit);
		}

		BigDecimal benodigdeIntakecapaciteit = totaalBenodigdeIntakecapaciteit.multiply(new BigDecimal(adherentie)).divide(BigDecimal.valueOf(10000), 4, RoundingMode.HALF_UP);
		return benodigdeIntakecapaciteit;
	}

	@Override
	public BigDecimal getIfobtFactorVoorGebied(UitnodigingsGebied uitnodigingsGebied, Integer percLandelijkIfobtRetour, Integer percLandelijkIfobtOngunstige)
	{
		Integer percIfobtRetour = percLandelijkIfobtRetour;

		if (uitnodigingsGebied.getGemeente().getScreeningOrganisatie() != null && uitnodigingsGebied.getGemeente().getScreeningOrganisatie().getIfobtRetourPercentage() != null)
		{
			percIfobtRetour = uitnodigingsGebied.getGemeente().getScreeningOrganisatie().getIfobtRetourPercentage();
		}

		if (uitnodigingsGebied.getPercentageIFobtRetour() != null)
		{
			percIfobtRetour = uitnodigingsGebied.getPercentageIFobtRetour();
		}

		BigDecimal ifobtRetourFactor = BigDecimal.ZERO;
		if (percIfobtRetour != null && percIfobtRetour > 0)
		{
			ifobtRetourFactor = BigDecimal.valueOf(10000).divide(BigDecimal.valueOf(percIfobtRetour), 4, RoundingMode.HALF_UP);
		}

		LOG.info("Uitnodigingsgebied " + uitnodigingsGebied.getNaam() + ": IfobtRetour "
			+ BigDecimalUtil.decimalToString(BigDecimal.valueOf(percIfobtRetour).divide(BigDecimal.valueOf(100), 4, RoundingMode.HALF_UP)) + "% => factor "
			+ BigDecimalUtil.decimalToString(ifobtRetourFactor));

		Integer percIfobtOngunstig = percLandelijkIfobtOngunstige;

		if (uitnodigingsGebied.getPercentageOngunstigeIfobt() != null)
		{
			percIfobtOngunstig = uitnodigingsGebied.getPercentageOngunstigeIfobt();
		}

		BigDecimal ifobtOngunstigFactor = BigDecimal.ZERO;
		if (percIfobtOngunstig != null && percIfobtOngunstig > 0)
		{
			ifobtOngunstigFactor = BigDecimal.valueOf(10000).divide(BigDecimal.valueOf(percIfobtOngunstig), 4, RoundingMode.HALF_UP);
		}

		LOG.info("Uitnodigingsgebied " + uitnodigingsGebied.getNaam() + ": IfobtOngunstig "
			+ BigDecimalUtil.decimalToString(BigDecimal.valueOf(percIfobtOngunstig).divide(BigDecimal.valueOf(100), 4, RoundingMode.HALF_UP)) + "% => factor "
			+ BigDecimalUtil.decimalToString(ifobtOngunstigFactor));

		return ifobtRetourFactor.multiply(ifobtOngunstigFactor);
	}

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public void wijzigingenDoorvoeren(UitnodigingsGebied uitnodigingsgebied, Map<String, Integer> newAdherentiePercentages,
		List<ColoscopieCentrumColonCapaciteitVerdeling> verwijderdeItems, List<CapaciteitsPercWijziging> capaciteitsPercWijzigingen, InstellingGebruiker ingelogdeGebruiker)
	{
		String melding = "Gewijzigde capaciteitspercentages:<br>";
		String adherentieMelding = "Adherentieverdeling gewijzigd voor gebied: " + uitnodigingsgebied.getNaam() + "<br>";
		for (CapaciteitsPercWijziging wijziging : capaciteitsPercWijzigingen)
		{
			ColoscopieCentrumColonCapaciteitVerdeling verdelingToChange = getVerdelingToChange(wijziging, uitnodigingsgebied);
			if (verdelingToChange != null)
			{
				if (wijziging.getNieuwCapPer().compareTo(verdelingToChange.getPercentageCapaciteit()) != 0)
				{
					melding += verdelingToChange.getUitnodigingsGebied().getNaam() + "/" + verdelingToChange.getColoscopieCentrum().getNaam() + ": "
						+ PercentageUtil.percentageToString(verdelingToChange.getPercentageCapaciteit()) + "->";

					verdelingToChange.setPercentageCapaciteit(wijziging.getNieuwCapPer());

					melding += PercentageUtil.percentageToString(verdelingToChange.getPercentageCapaciteit());
					if (verdelingToChange.getId() == null)
					{
						melding += "(nieuw)";
					}
					melding += "<br>";
				}
				Integer percentageAdherentie = newAdherentiePercentages.get(ColonRestrictions.getUniekIdOf(verdelingToChange));
				if (percentageAdherentie != null)
				{
					adherentieMelding += verdelingToChange.getColoscopieCentrum().getNaam() + ": " + PercentageUtil.percentageToString(verdelingToChange.getPercentageAdherentie())
						+ "->";
					verdelingToChange.setPercentageAdherentie(percentageAdherentie);
					adherentieMelding += PercentageUtil.percentageToString(verdelingToChange.getPercentageAdherentie()) + "<br>";
				}
				hibernateService.saveOrUpdate(verdelingToChange);
			}
		}

		hibernateService.saveOrUpdate(uitnodigingsgebied);

		for (ColoscopieCentrumColonCapaciteitVerdeling verwijderdeItem : verwijderdeItems)
		{
			melding += verwijderdeItem.getUitnodigingsGebied().getNaam() + "/" + verwijderdeItem.getColoscopieCentrum().getNaam() + " verwijderd";
			ColoscopieCentrum intakelocatie = verwijderdeItem.getColoscopieCentrum();
			uitnodigingsgebied.getVerdeling().remove(verwijderdeItem);
			hibernateService.saveOrUpdate(uitnodigingsgebied);
			intakelocatie.getCapaciteitVerdeling().remove(verwijderdeItem);
			hibernateService.delete(verwijderdeItem);
			hibernateService.saveOrUpdate(intakelocatie);
		}

		logService.logGebeurtenis(LogGebeurtenis.ADHERENTIE_AANEGEPAST, ingelogdeGebruiker, adherentieMelding + melding, Bevolkingsonderzoek.COLON);
		hibernateService.getHibernateSession().flush();
	}

	private ColoscopieCentrumColonCapaciteitVerdeling getVerdelingToChange(CapaciteitsPercWijziging wijziging, UitnodigingsGebied uitnodigingsgebied)
	{
		ColoscopieCentrumColonCapaciteitVerdeling verdelingToChange = null;
		main: for (ColoscopieCentrumColonCapaciteitVerdeling verdeling : uitnodigingsgebied.getVerdeling())
		{
			verdelingToChange = getVerdelingToChange(wijziging, verdeling);
			if (verdelingToChange != null)
			{
				break;
			}
			for (ColoscopieCentrumColonCapaciteitVerdeling innerVerdeling : verdeling.getColoscopieCentrum().getCapaciteitVerdeling())
			{
				verdelingToChange = getVerdelingToChange(wijziging, innerVerdeling);
				if (verdelingToChange != null)
				{
					break main;
				}
			}
		}
		return verdelingToChange;
	}

	private ColoscopieCentrumColonCapaciteitVerdeling getVerdelingToChange(CapaciteitsPercWijziging wijziging, ColoscopieCentrumColonCapaciteitVerdeling verdeling)
	{
		ColoscopieCentrumColonCapaciteitVerdeling verdelingToChange = null;
		if (verdeling.getId() != null)
		{
			if (verdeling.getId().equals(wijziging.getIlUgId()))
			{
				verdelingToChange = verdeling;
			}
		}
		else if (verdeling.getUitnodigingsGebied().getId().equals(wijziging.getUgId()) && verdeling.getColoscopieCentrum().getId().equals(wijziging.getIlId()))
		{
			verdelingToChange = verdeling;
		}
		return verdelingToChange;
	}

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public void wijzigingenDoorvoeren(ColoscopieCentrum intakelocatie, List<ColoscopieCentrumColonCapaciteitVerdeling> verwijderdeItems,
		List<CapaciteitsPercWijziging> capaciteitsPercWijzigingen, InstellingGebruiker ingelogdeGebruiker)
	{
		String melding = "";
		String adherentieMelding = "Verdeling gewijzigd voor intakelocatie: " + intakelocatie.getNaam() + "<br>";
		for (CapaciteitsPercWijziging wijziging : capaciteitsPercWijzigingen)
		{
			ColoscopieCentrumColonCapaciteitVerdeling verdelingToChange = null;
			for (ColoscopieCentrumColonCapaciteitVerdeling verdeling : intakelocatie.getCapaciteitVerdeling())
			{
				verdelingToChange = getVerdelingToChange(wijziging, verdeling);

				if (verdelingToChange == null)
				{
					verdelingToChange = getVerdelingToChange(wijziging, verdeling.getUitnodigingsGebied());
				}
				if (verdelingToChange != null)
				{
					if (verwijderdeItems.contains(verdelingToChange))
					{
						verdelingToChange = null;
					}
					break;
				}
			}
			if (verdelingToChange != null)
			{
				if (wijziging.getNieuwCapPer().compareTo(verdelingToChange.getPercentageCapaciteit()) != 0)
				{
					melding += "Capaciteit: " + verdelingToChange.getUitnodigingsGebied().getNaam() + "/" + verdelingToChange.getColoscopieCentrum().getNaam() + ": "
						+ PercentageUtil.percentageToString(verdelingToChange.getPercentageCapaciteit()) + "->";

					verdelingToChange.setPercentageCapaciteit(wijziging.getNieuwCapPer());

					melding += PercentageUtil.percentageToString(verdelingToChange.getPercentageCapaciteit());
					if (verdelingToChange.getId() == null)
					{
						melding += "(nieuw)";
					}
					melding += "<br>";
				}
				if (wijziging.getNieuwAdhPer().compareTo(verdelingToChange.getPercentageAdherentie()) != 0)
				{
					melding += "Adherentie: " + verdelingToChange.getUitnodigingsGebied().getNaam() + "/" + verdelingToChange.getColoscopieCentrum().getNaam() + ": "
						+ PercentageUtil.percentageToString(verdelingToChange.getPercentageAdherentie()) + "->";

					verdelingToChange.setPercentageAdherentie(wijziging.getNieuwAdhPer());

					melding += PercentageUtil.percentageToString(verdelingToChange.getPercentageAdherentie());
					if (verdelingToChange.getId() == null)
					{
						melding += "(nieuw)";
					}
					melding += "<br>";
				}
				hibernateService.saveOrUpdate(verdelingToChange);
			}
		}

		hibernateService.saveOrUpdate(intakelocatie);
		hibernateService.getHibernateSession().flush();

		for (ColoscopieCentrumColonCapaciteitVerdeling verwijderdeItem : verwijderdeItems)
		{
			melding += verwijderdeItem.getUitnodigingsGebied().getNaam() + "/" + verwijderdeItem.getColoscopieCentrum().getNaam() + " verwijderd";
			UitnodigingsGebied uitnodigingsGebied = verwijderdeItem.getUitnodigingsGebied();
			intakelocatie.getCapaciteitVerdeling().remove(verwijderdeItem);
			hibernateService.saveOrUpdate(intakelocatie);
			uitnodigingsGebied.getVerdeling().remove(verwijderdeItem);
			hibernateService.delete(verwijderdeItem);
			hibernateService.saveOrUpdate(uitnodigingsGebied);
		}

		logService.logGebeurtenis(LogGebeurtenis.ADHERENTIE_AANEGEPAST, ingelogdeGebruiker, adherentieMelding + melding, Bevolkingsonderzoek.COLON);
		hibernateService.getHibernateSession().flush();
	}

	@Override
	public List<CapaciteitsPercWijziging> bepaalCapaciteitsWijzigingen(ColoscopieCentrum intakelocatieMain, Map<String, Integer> newAdherentiePercentages,
		List<ColoscopieCentrumColonCapaciteitVerdeling> verwijderdeItems)
	{
		List<CapaciteitsPercWijziging> capaciteitsWijzigingen = new ArrayList<>();

		Map<Long, BigDecimal> benodigdeIntakecapaciteiten = new HashMap<>();

		Integer percLandelijkIfobtRetour = simplePreferenceService.getInteger(PreferenceKey.PERCENTAGEIFOBTRETOUR.name());
		if (percLandelijkIfobtRetour == null)
		{
			throw new IllegalStateException("Landelijk IfobtRetourPercentage is niet gezet");
		}

		Integer percLandelijkIfobtOngunstige = simplePreferenceService.getInteger(PreferenceKey.PERCENTGAGEIFOBTONGUSTIG.name());
		if (percLandelijkIfobtOngunstige == null)
		{
			throw new IllegalStateException("Landelijk IfobtOngunstigePercentage is niet gezet");
		}

		boolean eenGebied = intakelocatieMain.getCapaciteitVerdeling().size() == 1
			&& intakelocatieMain.getCapaciteitVerdeling().get(0).getUitnodigingsGebied().getVerdeling().size() == 1;
		List<String> invisibleAdherentieChanges = new ArrayList<>();

		for (ColoscopieCentrumColonCapaciteitVerdeling verdelingIntakelocatie : intakelocatieMain.getCapaciteitVerdeling())
		{
			Integer adherentie = getAdherentiePercentage(newAdherentiePercentages, verwijderdeItems, verdelingIntakelocatie);

			if (eenGebied && !adherentie.equals(Integer.valueOf(10000)) && !adherentie.equals(Integer.valueOf(0)))
			{
				throw new IllegalStateException("error.totaal.adherentie.niet.100.of.0.procent");
			}
			Integer adherentieVerschil = verdelingIntakelocatie.getPercentageAdherentie() - adherentie;
			if (!adherentieVerschil.equals(Integer.valueOf(0)))
			{
				UitnodigingsGebied uitnodigingsGebied = verdelingIntakelocatie.getUitnodigingsGebied();

				Integer totaalAdherentie = Integer.valueOf(0);
				for (ColoscopieCentrumColonCapaciteitVerdeling subSubverdeling : uitnodigingsGebied.getVerdeling())
				{
					if (!intakelocatieMain.equals(subSubverdeling.getColoscopieCentrum()))
					{
						totaalAdherentie += subSubverdeling.getPercentageAdherentie();
					}
				}
				if (totaalAdherentie.equals(Integer.valueOf(0)) && !adherentieVerschil.equals(Integer.valueOf(-10000)))
				{
					throw new IllegalStateException("error.adherentie.kan.niet.verdelen," + uitnodigingsGebied.getNaam());
				}
				for (ColoscopieCentrumColonCapaciteitVerdeling subSubverdeling : uitnodigingsGebied.getVerdeling())
				{
					if (!intakelocatieMain.equals(subSubverdeling.getColoscopieCentrum()))
					{
						Integer huidigeAdherentiePercentage = getAdherentiePercentage(newAdherentiePercentages, verwijderdeItems, subSubverdeling);
						BigDecimal newAdherentie = BigDecimal.valueOf(huidigeAdherentiePercentage).add(BigDecimal.valueOf(huidigeAdherentiePercentage)
							.multiply(BigDecimal.valueOf(adherentieVerschil)).divide(BigDecimal.valueOf(totaalAdherentie), RoundingMode.HALF_UP));
						newAdherentiePercentages.put(ColonRestrictions.getUniekIdOf(subSubverdeling), newAdherentie.intValue());
						invisibleAdherentieChanges.add(ColonRestrictions.getUniekIdOf(subSubverdeling));
					}
				}
				for (ColoscopieCentrumColonCapaciteitVerdeling subVerdeling : uitnodigingsGebied.getVerdeling())
				{
					ColoscopieCentrum intakelocatieSub = subVerdeling.getColoscopieCentrum();

					berekenVerschil(newAdherentiePercentages, verwijderdeItems, capaciteitsWijzigingen, benodigdeIntakecapaciteiten, percLandelijkIfobtRetour,
						percLandelijkIfobtOngunstige, intakelocatieSub.getCapaciteitVerdeling());
				}
			}
		}

		if (capaciteitsWijzigingen.isEmpty())
		{
			throw new IllegalStateException("error.adherentie.geen.wijzigingen");
		}

		Collections.sort(capaciteitsWijzigingen, new Comparator<CapaciteitsPercWijziging>()
		{
			@Override
			public int compare(CapaciteitsPercWijziging o1, CapaciteitsPercWijziging o2)
			{
				int result = o1.getUitnodigingsgebied().compareTo(o2.getUitnodigingsgebied());
				if (result == 0)
				{
					result = o1.getIntakelocatie().compareTo(o2.getIntakelocatie());
				}
				return result;
			}

		});
		for (String invisibleAdherentie : invisibleAdherentieChanges)
		{
			newAdherentiePercentages.remove(invisibleAdherentie);
		}
		return capaciteitsWijzigingen;
	}

	private Integer getAdherentiePercentage(Map<String, Integer> newAdherentiePercentages, List<ColoscopieCentrumColonCapaciteitVerdeling> verwijderdeItems,
		ColoscopieCentrumColonCapaciteitVerdeling verdelingIntakelocatie)
	{
		Integer adherentie = newAdherentiePercentages.get(ColonRestrictions.getUniekIdOf(verdelingIntakelocatie));
		if (adherentie == null)
		{
			adherentie = verdelingIntakelocatie.getPercentageAdherentie();
		}
		if (verwijderdeItems.contains(verdelingIntakelocatie))
		{
			adherentie = 0;
		}
		return adherentie;
	}

	private void berekenVerschil(Map<String, Integer> newAdherentiePercentages, List<ColoscopieCentrumColonCapaciteitVerdeling> verwijderdeItems,
		List<CapaciteitsPercWijziging> capaciteitsWijzigingen, Map<Long, BigDecimal> benodigdeIntakecapaciteiten, Integer percLandelijkIfobtRetour,
		Integer percLandelijkIfobtOngunstige, List<ColoscopieCentrumColonCapaciteitVerdeling> verdeling)
	{
		Map<Long, BigDecimal> benodigdeIntakecapaciteitenInLocatie = new HashMap<>();
		BigDecimal totaalIntakecapaciteitIntakelocatie = BigDecimal.ZERO;
		for (ColoscopieCentrumColonCapaciteitVerdeling subVerdeling : verdeling)
		{
			Integer adherentie = getAdherentiePercentage(newAdherentiePercentages, verwijderdeItems, subVerdeling);
			BigDecimal berekendeBenodigdeIntakecapaciteitVoorGebied = berekenBenodigdeIntakecapaciteitVoorGebied(subVerdeling.getUitnodigingsGebied(), adherentie,
				benodigdeIntakecapaciteiten, percLandelijkIfobtRetour, percLandelijkIfobtOngunstige);
			totaalIntakecapaciteitIntakelocatie = totaalIntakecapaciteitIntakelocatie.add(berekendeBenodigdeIntakecapaciteitVoorGebied);
			benodigdeIntakecapaciteitenInLocatie.put(subVerdeling.getUitnodigingsGebied().getId(), berekendeBenodigdeIntakecapaciteitVoorGebied);
		}

		for (ColoscopieCentrumColonCapaciteitVerdeling subVerdeling : verdeling)
		{
			CapaciteitsPercWijziging wijziging = new CapaciteitsPercWijziging();
			wijziging.setIlUgId(subVerdeling.getId());
			wijziging.setUgId(subVerdeling.getUitnodigingsGebied().getId());
			wijziging.setUitnodigingsgebied(subVerdeling.getUitnodigingsGebied().getNaam());
			ColoscopieCentrum subIntakelocatie = subVerdeling.getColoscopieCentrum();
			wijziging.setIlId(subIntakelocatie.getId());
			wijziging.setIntakelocatie(subIntakelocatie.getNaam());
			wijziging.setOudCapPer(subVerdeling.getPercentageCapaciteit());

			wijziging.setOudBerekendeIntakes(berekenBenodigdeIntakecapaciteitVoorGebied(subVerdeling.getUitnodigingsGebied(), subVerdeling.getPercentageAdherentie(),
				benodigdeIntakecapaciteiten, percLandelijkIfobtRetour, percLandelijkIfobtOngunstige));

			BigDecimal berekendeBenodigdeIntakecapaciteitVoorGebied = benodigdeIntakecapaciteitenInLocatie.get(subVerdeling.getUitnodigingsGebied().getId());

			BigDecimal nieuweCapaciteitspercentage = BigDecimal.ZERO;
			if (totaalIntakecapaciteitIntakelocatie.compareTo(BigDecimal.ZERO) > 0)
			{
				nieuweCapaciteitspercentage = berekendeBenodigdeIntakecapaciteitVoorGebied.multiply(BigDecimal.valueOf(10000)).divide(totaalIntakecapaciteitIntakelocatie, 4,
					RoundingMode.HALF_UP);
			}

			wijziging.setNieuwCapPer(nieuweCapaciteitspercentage.intValue());
			wijziging.setNieuwBerekendeIntakes(berekendeBenodigdeIntakecapaciteitVoorGebied);
			wijziging.setOudAdhPer(subVerdeling.getPercentageAdherentie());
			wijziging.setNieuwAdhPer(getAdherentiePercentage(newAdherentiePercentages, verwijderdeItems, subVerdeling));
			Integer aantalGeprognostiseerdeRoosterblokken = subIntakelocatie.getAantalGeprognostiseerdeRoosterblokken();
			if (aantalGeprognostiseerdeRoosterblokken != null)
			{

				DateTime nu = currentDateSupplier.getDateTimeMidnight();
				DateTime startYear = nu.withDayOfYear(1);
				int daysInCurYear = Days.daysBetween(startYear, startYear.plusYears(1)).getDays();
				int restDaysInCurYear = Days.daysBetween(nu, startYear.plusYears(1)).getDays();
				BigDecimal prognose = new BigDecimal(aantalGeprognostiseerdeRoosterblokken).multiply(new BigDecimal(restDaysInCurYear)).divide(new BigDecimal(daysInCurYear), 2,
					RoundingMode.HALF_UP);

				wijziging
					.setOudIntakesProg(prognose.multiply(BigDecimal.valueOf(subVerdeling.getPercentageCapaciteit())).divide(BigDecimal.valueOf(10000), 2, RoundingMode.HALF_UP));
				wijziging.setNieuwIntakesProg(prognose.multiply(nieuweCapaciteitspercentage).divide(BigDecimal.valueOf(10000), 2, RoundingMode.HALF_UP));
			}

			capaciteitsWijzigingen.remove(wijziging);
			capaciteitsWijzigingen.add(wijziging);
		}
	}

	@Override
	public List<UitnodigingsGebied> getAllUitnodigingsgebieden()
	{
		return hibernateService.loadAll(UitnodigingsGebied.class, "naam", true);
	}

}
