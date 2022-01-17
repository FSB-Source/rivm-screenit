package nl.rivm.screenit.mamma.planning.controller;

/*-
 * ========================LICENSE_START=================================
 * screenit-planning-bk
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

import java.math.BigDecimal;
import java.time.LocalDate;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.NavigableSet;
import java.util.OptionalDouble;
import java.util.Set;
import java.util.TreeSet;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;
import java.util.stream.Collectors;

import nl.rivm.screenit.PreferenceKey;
import nl.rivm.screenit.dto.mamma.planning.PlanningRestConstants;
import nl.rivm.screenit.mamma.planning.dao.PlanningReadModelDao;
import nl.rivm.screenit.mamma.planning.index.PlanningScreeningsEenheidIndex;
import nl.rivm.screenit.mamma.planning.index.PlanningStatusIndex;
import nl.rivm.screenit.mamma.planning.model.PlanningClient;
import nl.rivm.screenit.mamma.planning.model.PlanningConstanten;
import nl.rivm.screenit.mamma.planning.model.PlanningDag;
import nl.rivm.screenit.mamma.planning.model.PlanningPostcodeReeks;
import nl.rivm.screenit.mamma.planning.model.PlanningPostcodeReeksRegio;
import nl.rivm.screenit.mamma.planning.model.PlanningScreeningsEenheid;
import nl.rivm.screenit.mamma.planning.model.PlanningScreeningsOrganisatie;
import nl.rivm.screenit.mamma.planning.model.PlanningStandplaats;
import nl.rivm.screenit.mamma.planning.model.PlanningStandplaatsPeriode;
import nl.rivm.screenit.mamma.planning.model.PlanningStandplaatsRonde;
import nl.rivm.screenit.mamma.planning.model.PlanningTehuis;
import nl.rivm.screenit.mamma.planning.model.PopulatieMetStreefDatum;
import nl.rivm.screenit.mamma.planning.model.rapportage.PlanningStandplaatsRondeUitnodigenRapportageDto;
import nl.rivm.screenit.mamma.planning.model.rapportage.PlanningUitnodigenRapportageDto;
import nl.rivm.screenit.mamma.planning.service.PlanningUitnodigenService;
import nl.rivm.screenit.mamma.planning.service.PlanningUitnodigingContext;
import nl.rivm.screenit.mamma.planning.service.impl.UitnodigenCapaciteitCalculator;
import nl.rivm.screenit.model.Instelling;
import nl.rivm.screenit.model.Rivm;
import nl.rivm.screenit.model.ScreeningOrganisatie;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.model.logging.LogEvent;
import nl.rivm.screenit.model.mamma.MammaScreeningsEenheid;
import nl.rivm.screenit.model.mamma.MammaStandplaatsPeriode;
import nl.rivm.screenit.model.mamma.MammaStandplaatsRonde;
import nl.rivm.screenit.model.mamma.enums.MammaDoelgroep;
import nl.rivm.screenit.model.mamma.enums.MammaPlanningStatus;
import nl.rivm.screenit.model.mamma.enums.MammaUitstelReden;
import nl.rivm.screenit.model.verwerkingverslag.mamma.MammaStandplaatsPeriodeUitnodigenRapportage;
import nl.rivm.screenit.model.verwerkingverslag.mamma.MammaStandplaatsRondeRapportageStatus;
import nl.rivm.screenit.model.verwerkingverslag.mamma.MammaStandplaatsRondeUitnodigenRapportage;
import nl.rivm.screenit.model.verwerkingverslag.mamma.MammaUitnodigenRapportage;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.InstellingService;
import nl.rivm.screenit.service.LogService;
import nl.rivm.screenit.util.DateUtil;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.hibernate.spring.services.impl.OpenHibernate5Session;
import nl.topicuszorg.preferencemodule.service.SimplePreferenceService;

import org.hibernate.FlushMode;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequestMapping("/" + PlanningRestConstants.C_UITNODIGEN)
public class PlanningUitnodigenController
{
	private static final Logger LOG = LoggerFactory.getLogger(PlanningUitnodigenController.class);

	@Autowired
	private PlanningReadModelDao readModelDao;

	@Autowired
	private PlanningUitnodigenService uitnodigenService;

	@Autowired
	private SimplePreferenceService preferenceService;

	@Autowired
	private ICurrentDateSupplier dateSupplier;

	@Autowired
	private HibernateService hibernateService;

	@Autowired
	private LogService logService;

	@Autowired
	private InstellingService instellingService;

	private static final ThreadPoolExecutor executor = new ThreadPoolExecutor(0, 100, 0, TimeUnit.MILLISECONDS, new LinkedBlockingQueue<>());

	private static final BigDecimal MINUS_HALF = BigDecimal.valueOf(-0.5);

	private static Integer uitnodigenVanafJaar;

	@RequestMapping
	public Long uitnodigen()
	{
		try
		{
			PlanningStatusIndex.set(MammaPlanningStatus.UITNODIGEN);
			uitnodigenService.clear();
			uitnodigenVanafJaar = dateSupplier.getLocalDate().getYear();

			readModelDao.readDataModel();

			hibernateService.getHibernateSession().setFlushMode(FlushMode.COMMIT);

			MammaUitnodigenRapportage rapportage = new MammaUitnodigenRapportage();

			uitnodigen(rapportage);

			readModelDao.readDataModel();

			rapportage.setDatumVerwerking(dateSupplier.getDate());
			hibernateService.save(rapportage);
			hibernateService.saveOrUpdateAll(rapportage.getStandplaatsRondeUitnodigenRapportages());
			for (MammaStandplaatsRondeUitnodigenRapportage standplaatsRondeUitnodigenRapportage : rapportage.getStandplaatsRondeUitnodigenRapportages())
			{
				hibernateService.saveOrUpdateAll(standplaatsRondeUitnodigenRapportage.getStandplaatsPeriodeUitnodigenRapportages());
			}

			PlanningStatusIndex.set(MammaPlanningStatus.OPERATIONEEL);
			return rapportage.getId();
		}
		catch (Exception e)
		{
			LOG.error("Error tijdens uitnodigen", e);
			List<Instelling> instellingList = new ArrayList<>(instellingService.getAllActiefScreeningOrganisaties());
			Instelling rivm = instellingService.getActieveInstellingen(Rivm.class).get(0);
			instellingList.add(rivm);
			logService.logGebeurtenis(LogGebeurtenis.MAMMA_UITNODIGEN_FOUT, instellingList, new LogEvent("Technische fout bij uitnodigen"),
				Bevolkingsonderzoek.MAMMA);
			PlanningStatusIndex.set(MammaPlanningStatus.ERROR);
			return null;
		}
	}

	private void uitnodigen(MammaUitnodigenRapportage rapportage)
	{
		Integer afspraakVanafAantalWerkdagen = preferenceService.getInteger(PreferenceKey.MAMMA_AFSPRAAK_BIJ_UITNODIGEN_VANAF_AANTAL_WERKDAGEN.name());
		LocalDate afsprakenVanafDatum = Collections
			.max(Arrays.asList(DateUtil.plusWerkdagen(PlanningConstanten.prognoseVanafDatum, afspraakVanafAantalWerkdagen), PlanningConstanten.plannenVanafDatum));

		NavigableSet<PlanningStandplaatsRonde> standplaatsRondeNavigableSet = new TreeSet<>((standplaatsRonde1, standplaatsRonde2) -> {
			LocalDate vanaf1 = standplaatsRonde1.getStandplaatsPeriodeNavigableSet().first().getVanaf();
			LocalDate vanaf2 = standplaatsRonde2.getStandplaatsPeriodeNavigableSet().first().getVanaf();

			int compareTo = vanaf1.compareTo(vanaf2);
			if (vanaf1.compareTo(vanaf2) != 0)
			{
				return compareTo;
			}
			return Long.compare(standplaatsRonde1.getId(), standplaatsRonde2.getId());
		});
		for (PlanningScreeningsEenheid screeningsEenheid : PlanningScreeningsEenheidIndex.getScreeningsEenheden())
		{
			LocalDate uitnodigenTotEnMet = screeningsEenheid.getUitnodigenTotEnMet();
			if (uitnodigenTotEnMet != null)
			{
				for (LocalDate datum = PlanningConstanten.prognoseVanafDatum; !datum.isAfter(uitnodigenTotEnMet); datum = datum.plusDays(1))
				{
					PlanningDag dag = screeningsEenheid.getDagNavigableMap().get(datum);
					PlanningStandplaatsPeriode standplaatsPeriode = dag.getStandplaatsPeriode();
					if (standplaatsPeriode != null && !standplaatsPeriode.getStandplaatsRonde().getAchtervangToegepast())
					{
						standplaatsRondeNavigableSet.add(standplaatsPeriode.getStandplaatsRonde());
					}
				}
			}
		}

		executor.setCorePoolSize(Math.min(standplaatsRondeNavigableSet.size(), 30));
		PlanningUitnodigenRapportageDto rapportageDto = new PlanningUitnodigenRapportageDto();
		PlanningUitnodigingContext context = new PlanningUitnodigingContext(preferenceService, standplaatsRondeNavigableSet.size());

		standplaatsRondeNavigableSet.forEach(standplaatsRonde -> uitnodigen(standplaatsRonde, afsprakenVanafDatum, rapportageDto, context));
		try
		{
			context.onderbrekenCountDownLatch.await(160, TimeUnit.MINUTES); 
			long aantalRunningThreads = context.onderbrekenCountDownLatch.getCount();
			if (aantalRunningThreads > 0)
			{
				LOG.info("Max. tijd voorbij. #threads krijgen een soft stop: " + aantalRunningThreads);
			}
			context.uitnodigenOnderbreken = true;
			context.onderbrokenCountDownLatch.await();
			LOG.info("Alle threads klaar.");
		}
		catch (InterruptedException e)
		{
			LOG.error("Fout bij wachten op standplaatsRonde uitnodigen threads", e);
		}
		rapportageDtoToEntity(rapportageDto, rapportage);

		for (PlanningScreeningsEenheid screeningsEenheid : PlanningScreeningsEenheidIndex.getScreeningsEenheden())
		{
			if (screeningsEenheid.getUitnodigenTotEnMet() != null)
			{
				MammaScreeningsEenheid mammaScreeningsEenheid = hibernateService.get(MammaScreeningsEenheid.class, screeningsEenheid.getId());
				mammaScreeningsEenheid.setUitgenodigdTotEnMet(DateUtil.toUtilDate(screeningsEenheid.getUitnodigenTotEnMet()));
				hibernateService.saveOrUpdate(mammaScreeningsEenheid);
			}
		}
	}

	private void uitnodigen(PlanningStandplaatsRonde standplaatsRonde, LocalDate afsprakenVanafDatum, PlanningUitnodigenRapportageDto rapportageDto,
		PlanningUitnodigingContext context)
	{
		executor.submit(() -> {
			try
			{
				OpenHibernate5Session.withCommittedTransaction().run(() -> {
					LOG.info("uitnodigen standplaatsRonde: " + standplaatsRonde.getId());
					PlanningStandplaats standplaats = standplaatsRonde.getStandplaats();
					PlanningScreeningsOrganisatie screeningsOrganisatieStandplaats = standplaats.getScreeningsOrganisatie();
					PlanningStandplaatsPeriode laatsteStandplaatsPeriode = standplaatsRonde.getStandplaatsPeriodeNavigableSet().stream()
						.filter(standplaatsPeriode -> standplaatsPeriode.getScreeningsEenheid().getUitnodigenTotEnMet() != null)
						.max(PlanningStandplaatsRonde.getStandplaatsPeriodeComparator()).orElse(null);

					MammaStandplaatsPeriode mammaStandplaatsPeriode = hibernateService.get(MammaStandplaatsPeriode.class, laatsteStandplaatsPeriode.getId());
					MammaStandplaatsRonde mammaStandplaatsRonde = hibernateService.get(MammaStandplaatsRonde.class, standplaatsRonde.getId());

					uitnodigenService.getStandplaatsPeriodeUitnodigenRapportage(rapportageDto, mammaStandplaatsPeriode);

					boolean achtervang = !standplaatsRonde.getAchtervangToegepast() && !afsprakenVanafDatum.isBefore(laatsteStandplaatsPeriode.getTotEnMet());

					final int uitnodigenTotEnMetJaar = laatsteStandplaatsPeriode.getTotEnMet().getYear();

					Map<String, PopulatieMetStreefDatum> populatiePerWijk = selecteerPopulatiePerWijk(standplaats, uitnodigenTotEnMetJaar, context);

					NavigableSet<PopulatieMetStreefDatum> standplaatsPopulatie = bepaalStandplaatsPopulatieOpStreefDatum(populatiePerWijk);

					for (PlanningClient client : standplaats.getUitstelSet())
					{
						client.setHuidigeStreefDatum(client.getUitstelStreefDatum());

						standplaatsPopulatie.add(PopulatieMetStreefDatum.voorUitstelClient(client));
					}

					Set<PlanningClient> achtervangUitstelSet = new HashSet<>();
					Set<PlanningClient> minderValideUitwijkUitstelSet = new HashSet<>();
					Set<PlanningClient> uitTeNodigenClientSet = new HashSet<>();
					boolean uitnodigen = !afsprakenVanafDatum.isAfter(laatsteStandplaatsPeriode.getTotEnMet());
					BigDecimal extraMindervalideCapaciteitUitgenodigd = mammaStandplaatsRonde.getExtraMinderValideCapaciteitUitgenodigd();

					NavigableSet<PlanningDag> uitTeNodigenDagen = bepaalUitTeNodigenDagen(standplaatsRonde);

					UitnodigenCapaciteitCalculator capaciteitCalculator = new UitnodigenCapaciteitCalculator(uitTeNodigenDagen, standplaatsRonde, standplaatsPopulatie,
						extraMindervalideCapaciteitUitgenodigd);
					BigDecimal capaciteitVoorUitnodigen = capaciteitCalculator.berekenCapaciteitVoorUitnodigen();

					for (PopulatieMetStreefDatum populatieMetStreefDatum : standplaatsPopulatie)
					{
						for (PlanningClient client : populatieMetStreefDatum.getClienten())
						{
							if (uitTeNodigen(client))
							{
								if (mindervalideUitwijk(standplaatsRonde, uitnodigen, client))
								{
									minderValideUitwijkUitstelSet.add(client);
								}
								else
								{
									if (alleMinderValideUitnodigen(standplaatsRonde, client))
									{

										uitTeNodigenClientSet.add(client);
										extraMindervalideCapaciteitUitgenodigd = extraMindervalideCapaciteitUitgenodigd
											.add(client.getBenodigdeCapaciteit(screeningsOrganisatieStandplaats));
									}
									else if (achtervang)
									{
										achtervangUitstelSet.add(client);
									}
									else if (uitnodigen && isCapaciteitBeschikbaarVoor(client, capaciteitVoorUitnodigen, screeningsOrganisatieStandplaats))
									{
										capaciteitVoorUitnodigen = capaciteitVoorUitnodigen.subtract(client.getBenodigdeCapaciteit(screeningsOrganisatieStandplaats));
										uitTeNodigenClientSet.add(client);
									}
								}
							}
						}
					}

					BigDecimal afspraakDrempel = new BigDecimal(
						standplaatsRonde.getAfspraakDrempel() != null ? standplaatsRonde.getAfspraakDrempel() : screeningsOrganisatieStandplaats.getAfspraakDrempel())
							.movePointLeft(2);

					Set<PlanningClient> openUitnodigingClientSet = new HashSet<>();
					NavigableSet<PlanningClient> afspraakUitnodigingClientSet = new TreeSet<>((client1, client2) -> {
						int compareTo = client2.getDeelnamekans().compareTo(client1.getDeelnamekans());
						if (compareTo != 0)
						{
							return compareTo;
						}
						return client1.getId().compareTo(client2.getId());
					});

					for (PlanningClient client : uitTeNodigenClientSet)
					{
						if (client.getDoelgroep().equals(MammaDoelgroep.MINDER_VALIDE))
						{
							openUitnodigingClientSet.add(client);
						}
						else if (!client.isSuspect() && (client.getDeelnamekans().compareTo(afspraakDrempel) >= 0 || heeftUitstel(client)))
						{
							afspraakUitnodigingClientSet.add(client);
						}
						else
						{
							openUitnodigingClientSet.add(client);
						}
					}

					uitnodigenService.uitnodigen(standplaatsRonde,
						openUitnodigingClientSet,
						afspraakUitnodigingClientSet,
						rapportageDto, context);

					uitnodigenService.minderValideUitwijkUitstel(standplaatsRonde, minderValideUitwijkUitstelSet, rapportageDto);

					if (achtervang)
					{

						uitnodigenService.achtervangUitstel(standplaatsRonde, achtervangUitstelSet, rapportageDto);
					}

					rapportageBijwerken(rapportageDto, standplaatsRonde, standplaatsPopulatie, achtervangUitstelSet,
						minderValideUitwijkUitstelSet, context, uitnodigenTotEnMetJaar);

					mammaStandplaatsRonde.setExtraMinderValideCapaciteitUitgenodigd(extraMindervalideCapaciteitUitgenodigd);
					hibernateService.saveOrUpdate(mammaStandplaatsRonde);
				});
			}
			catch (Exception e)
			{
				LOG.error("Fout bij uitnodigen", e);

			}
			finally
			{
				context.onderbrekenCountDownLatch.countDown();
				context.onderbrokenCountDownLatch.countDown();
			}
		});
	}

	private NavigableSet<PlanningDag> bepaalUitTeNodigenDagen(PlanningStandplaatsRonde standplaatsRonde)
	{
		NavigableSet<PlanningDag> uitTeNodigenDagen = new TreeSet<>();
		for (PlanningStandplaatsPeriode standplaatsPeriode : standplaatsRonde.getStandplaatsPeriodeNavigableSet())
		{
			PlanningScreeningsEenheid screeningsEenheid = standplaatsPeriode.getScreeningsEenheid();
			LocalDate uitnodigenTotEnMet = screeningsEenheid.getUitnodigenTotEnMet();
			if (uitnodigenTotEnMet != null && !standplaatsPeriode.getVanaf().isAfter(uitnodigenTotEnMet))
			{
				uitTeNodigenDagen.addAll(screeningsEenheid.getDagNavigableMap()
					.subMap(standplaatsPeriode.getVanaf(), true, Collections.min(Arrays.asList(standplaatsPeriode.getTotEnMet(), uitnodigenTotEnMet)), true).values());
			}
		}
		return uitTeNodigenDagen;
	}

	private Map<String, PopulatieMetStreefDatum> selecteerPopulatiePerWijk(PlanningStandplaats standplaats, int uitnodigenTotEnMetJaar, PlanningUitnodigingContext context)
	{
		Map<String, PopulatieMetStreefDatum> populatiePerWijk = new HashMap<>();

		for (PlanningPostcodeReeks postcodeReeks : standplaats.getPostcodeReeksSet())
		{
			for (PlanningPostcodeReeksRegio wijk : postcodeReeks.getPostcodeReeksRegios())
			{
				PopulatieMetStreefDatum wijkPopulatie = populatiePerWijk.get(wijk.getCijfer());
				if (wijkPopulatie == null)
				{
					wijkPopulatie = new PopulatieMetStreefDatum();
					populatiePerWijk.put(wijk.getCijfer(), wijkPopulatie);
				}

				for (PlanningClient client : wijk.getClientSet())
				{
					if (teSelecteren(client, context, uitnodigenTotEnMetJaar, wijk))
					{
						wijkPopulatie.getClienten().add(client); 
					}
				}

				if (wijkPopulatie.getClienten().isEmpty())
				{
					populatiePerWijk.remove(wijk.getCijfer()); 
				}
			}
		}

		return populatiePerWijk;
	}

	private NavigableSet<PopulatieMetStreefDatum> bepaalStandplaatsPopulatieOpStreefDatum(Map<String, PopulatieMetStreefDatum> populatiePerWijk)
	{
		for (PopulatieMetStreefDatum wijkPopulatie : populatiePerWijk.values())
		{
			scaleHuidigeStreefDatumEersteRondeClienten(wijkPopulatie.getClienten());
			OptionalDouble average = wijkPopulatie.getClienten().stream().mapToLong(client -> client.getHuidigeStreefDatum().toEpochDay()).average();
			wijkPopulatie.setUitnodigingStreefDatum(LocalDate.ofEpochDay(Math.round(average.getAsDouble())));
		}
		return new TreeSet<>(populatiePerWijk.values());
	}

	public boolean isCapaciteitBeschikbaarVoor(PlanningClient client, BigDecimal capaciteitVoorUitnodigen, PlanningScreeningsOrganisatie screeningsOrganisatie)
	{
		BigDecimal benodigdeCapaciteit = client.getBenodigdeCapaciteit(screeningsOrganisatie);
		return capaciteitVoorUitnodigen.subtract(benodigdeCapaciteit).compareTo(MINUS_HALF.multiply(client.getDeelnamekans())) > 0;
	}

	private boolean alleMinderValideUitnodigen(PlanningStandplaatsRonde standplaatsRonde, PlanningClient client)
	{
		return client.getDoelgroep().equals(MammaDoelgroep.MINDER_VALIDE) && standplaatsRonde.getMinderValideUitnodigenVanaf() != null
			&& !dateSupplier.getLocalDate().isBefore(standplaatsRonde.getMinderValideUitnodigenVanaf());
	}

	private boolean mindervalideUitwijk(PlanningStandplaatsRonde standplaatsRonde, boolean uitnodigen, PlanningClient client)
	{
		return client.getDoelgroep().equals(MammaDoelgroep.MINDER_VALIDE) && uitnodigen && standplaatsRonde.getMinderValideUitwijkStandplaats() != null
			&& !MammaUitstelReden.MINDER_VALIDE_UITWIJK_UITSTEL.equals(client.getUitstelReden());
	}

	private static boolean teSelecteren(PlanningClient client, PlanningUitnodigingContext context, int uitnodigenTotEnMetJaar, PlanningPostcodeReeksRegio postcodeReeksRegio)
	{
		return client.getUitnodigenVanafJaar() <= uitnodigenTotEnMetJaar
			&& (client.getUitnodigenTotEnMetJaar() >= uitnodigenVanafJaar
				|| !(client.getDoelgroep() == MammaDoelgroep.MINDER_VALIDE || client.getTehuis() != null) && isUitgenodigdVorigJaar(postcodeReeksRegio.getClientSet()))
			&& (client.isUitgenodigdHuidigeStandplaatsRonde()
				|| client.getLaatsteMammografieAfgerondDatum() == null
				|| client.getLaatsteMammografieAfgerondDatum().plusDays(context.minimaleIntervalMammografieOnderzoeken)
					.isBefore(PlanningConstanten.prognoseVanafDatum))
			&& (client.getVorigeScreeningRondeCreatieDatum() == null
				|| client.getVorigeScreeningRondeCreatieDatum().plusDays(context.minimaleIntervalUitnodigingen)
					.isBefore(PlanningConstanten.prognoseVanafDatum))
			&& client.getUitstelStandplaats() == null;
	}

	private static boolean isUitgenodigdVorigJaar(Set<PlanningClient> clientSet)
	{
		return clientSet.stream().anyMatch(client -> client.isUitgenodigdHuidigeStandplaatsRonde()
			&& !client.getDoelgroep().equals(MammaDoelgroep.MINDER_VALIDE)
			&& !client.isUitgenodigdHuidigeStandplaatsRondeIsGeforceerd()
			&& client.getLaatsteScreeningRondeCreatieDatum().getYear() < uitnodigenVanafJaar);
	}

	private static boolean uitTeNodigen(PlanningClient client)
	{
		return client.getAfspraakStandplaats() == null && client.getUitstelStandplaats() == null && !client.isUitgenodigdHuidigeStandplaatsRonde()
			|| Boolean.FALSE.equals(client.getUitgenodigdNaUitstel());
	}

	private static boolean heeftUitstel(PlanningClient client)
	{
		return client.getUitstelStandplaats() != null && client.getUitstelReden() == MammaUitstelReden.CLIENT_CONTACT && !client.getUitgenodigdNaUitstel();
	}

	private void rapportageDtoToEntity(PlanningUitnodigenRapportageDto rapportageDto, MammaUitnodigenRapportage rapportage)
	{
		Instelling rivm = instellingService.getActieveInstellingen(Rivm.class).get(0);
		rapportageDto.getStandplaatsRondeUitnodigenRapportages().forEach(standplaatsRondeUitnodigingRapportageDto -> {
			MammaStandplaatsRondeUitnodigenRapportage standplaatsRondeUitnodigenRapportage = new MammaStandplaatsRondeUitnodigenRapportage();
			standplaatsRondeUitnodigenRapportage
				.setStandplaatsRonde(hibernateService.load(MammaStandplaatsRonde.class, standplaatsRondeUitnodigingRapportageDto.getStandplaatsRondeId()));
			standplaatsRondeUitnodigenRapportage.setStatus(standplaatsRondeUitnodigingRapportageDto.getStatus());
			standplaatsRondeUitnodigenRapportage.setTotaalDubbeleTijd(standplaatsRondeUitnodigingRapportageDto.getTotaalDubbeleTijd());
			standplaatsRondeUitnodigenRapportage.setTotaalMinderValide(standplaatsRondeUitnodigingRapportageDto.getTotaalMinderValide());
			standplaatsRondeUitnodigenRapportage.setTotaalSuspect(standplaatsRondeUitnodigingRapportageDto.getTotaalSuspect());
			standplaatsRondeUitnodigenRapportage.setTotaalTehuis(standplaatsRondeUitnodigingRapportageDto.getTotaalTehuis());
			standplaatsRondeUitnodigenRapportage.setTotaalTotaal(standplaatsRondeUitnodigingRapportageDto.getTotaalTotaal());
			standplaatsRondeUitnodigenRapportage.setTotaalVervolgRonde(standplaatsRondeUitnodigingRapportageDto.getTotaalVervolgRonde());
			standplaatsRondeUitnodigenRapportage.setTotaalEersteRonde(standplaatsRondeUitnodigingRapportageDto.getTotaalEersteRonde());
			standplaatsRondeUitnodigenRapportage.setUitTeNodigenTotaal(standplaatsRondeUitnodigingRapportageDto.getUitTeNodigenTotaal());
			standplaatsRondeUitnodigenRapportage.setUitTeNodigenDubbeleTijd(standplaatsRondeUitnodigingRapportageDto.getUitTeNodigenDubbeleTijd());
			standplaatsRondeUitnodigenRapportage.setUitTeNodigenEersteRonde(standplaatsRondeUitnodigingRapportageDto.getUitTeNodigenEersteRonde());
			standplaatsRondeUitnodigenRapportage.setUitTeNodigenMinderValide(standplaatsRondeUitnodigingRapportageDto.getUitTeNodigenMinderValide());
			standplaatsRondeUitnodigenRapportage.setUitTeNodigenSuspect(standplaatsRondeUitnodigingRapportageDto.getUitTeNodigenSuspect());
			standplaatsRondeUitnodigenRapportage.setUitTeNodigenTehuis(standplaatsRondeUitnodigingRapportageDto.getUitTeNodigenTehuis());
			standplaatsRondeUitnodigenRapportage.setUitTeNodigenVervolgRonde(standplaatsRondeUitnodigingRapportageDto.getUitTeNodigenVervolgRonde());
			standplaatsRondeUitnodigenRapportage.setUitnodigenRapportage(rapportage);
			List<MammaStandplaatsPeriodeUitnodigenRapportage> standplaatsPeriodeUitnodigenRapportages = standplaatsRondeUitnodigenRapportage
				.getStandplaatsPeriodeUitnodigenRapportages();
			standplaatsRondeUitnodigingRapportageDto.getStandplaatsPeriodeUitnodigenRapportages().forEach(standplaatsPeriodeUitnodigenRapportageDto -> {
				boolean uitnodigenFout = MammaStandplaatsRondeRapportageStatus.FOUT.equals(standplaatsRondeUitnodigenRapportage.getStatus());
				MammaStandplaatsPeriodeUitnodigenRapportage standplaatsPeriodeUitnodigenRapportage = new MammaStandplaatsPeriodeUitnodigenRapportage();
				standplaatsPeriodeUitnodigenRapportage
					.setStandplaatsPeriode(hibernateService.load(MammaStandplaatsPeriode.class, standplaatsPeriodeUitnodigenRapportageDto.getStandplaatsPeriodeId()));
				standplaatsPeriodeUitnodigenRapportage.setStandplaatsRondeUitnodigenRapportage(standplaatsRondeUitnodigenRapportage);
				standplaatsPeriodeUitnodigenRapportage.setUitgenodigdAfspraak(!uitnodigenFout ? standplaatsPeriodeUitnodigenRapportageDto.getUitgenodigdAfspraak() : -1);
				standplaatsPeriodeUitnodigenRapportage.setUitgenodigdMinderValide(!uitnodigenFout ? standplaatsPeriodeUitnodigenRapportageDto.getUitgenodigdMinderValide() : -1);
				standplaatsPeriodeUitnodigenRapportage.setUitgenodigdNaUitstel(!uitnodigenFout ? standplaatsPeriodeUitnodigenRapportageDto.getUitgenodigdNaUitstel() : -1);
				standplaatsPeriodeUitnodigenRapportage.setUitgenodigdOpen(!uitnodigenFout ? standplaatsPeriodeUitnodigenRapportageDto.getUitgenodigdOpen() : -1);
				standplaatsPeriodeUitnodigenRapportage.setUitgenodigdSuspect(!uitnodigenFout ? standplaatsPeriodeUitnodigenRapportageDto.getUitgenodigdSuspect() : -1);
				standplaatsPeriodeUitnodigenRapportage.setUitnodigenTotEnMet(standplaatsPeriodeUitnodigenRapportageDto.getUitnodigenTotEnMet());
				standplaatsPeriodeUitnodigenRapportage
					.setUitgesteldAchtervangUitstel(!uitnodigenFout ? standplaatsPeriodeUitnodigenRapportageDto.getUitgesteldAchtervangUitstel() : -1);
				standplaatsPeriodeUitnodigenRapportage
					.setUitgesteldMinderValideUitgewijktUitstel(!uitnodigenFout ? standplaatsPeriodeUitnodigenRapportageDto.getUitgesteldMinderValideUitgewijktUitstel() : -1);
				standplaatsPeriodeUitnodigenRapportages.add(standplaatsPeriodeUitnodigenRapportage);

				MammaStandplaatsRondeRapportageStatus rondeRapportageStatus = standplaatsRondeUitnodigingRapportageDto.getStatus();
				if (!MammaStandplaatsRondeRapportageStatus.VOLTOOID.equals(rondeRapportageStatus))
				{
					MammaStandplaatsRonde mammaStandplaatsRonde = standplaatsRondeUitnodigenRapportage.getStandplaatsRonde();
					ScreeningOrganisatie regio = mammaStandplaatsRonde.getStandplaats().getRegio();

					if (MammaStandplaatsRondeRapportageStatus.FOUT.equals(rondeRapportageStatus))
					{
						logService.logGebeurtenis(LogGebeurtenis.MAMMA_UITNODIGEN_FOUT, Arrays.asList(rivm, regio),
							new LogEvent("Technische fout bij uitnodigen voor standplaats: " + mammaStandplaatsRonde.getStandplaats().getNaam()),
							Bevolkingsonderzoek.MAMMA);
					}
					else if (MammaStandplaatsRondeRapportageStatus.ONDERBROKEN.equals(rondeRapportageStatus))
					{
						logService.logGebeurtenis(LogGebeurtenis.MAMMA_UITNODIGEN_ONDERBROKEN, Arrays.asList(rivm, regio),
							new LogEvent("Uitnodigen onderbroken door maximale tijdsduur batch voor standplaats: " + mammaStandplaatsRonde.getStandplaats().getNaam()),
							Bevolkingsonderzoek.MAMMA);
					}
				}
			});
			rapportage.getStandplaatsRondeUitnodigenRapportages().add(standplaatsRondeUitnodigenRapportage);

		});
	}

	private void rapportageBijwerken(PlanningUitnodigenRapportageDto rapportageDto, PlanningStandplaatsRonde standplaatsRonde,
		NavigableSet<PopulatieMetStreefDatum> clientSetStreefDatumNavigableSet, Set<PlanningClient> achtervangUitstelSet,
		Set<PlanningClient> mindervalideUitwijkUitstelSet, PlanningUitnodigingContext context, int uitnodigenTotEnMetJaar)
	{
		Set<PlanningClient> clientSet = new HashSet<>();
		for (PopulatieMetStreefDatum populatieMetStreefDatum : clientSetStreefDatumNavigableSet)
		{
			clientSet.addAll(populatieMetStreefDatum.getClienten());
		}
		for (PlanningTehuis tehuis : standplaatsRonde.getStandplaats().getTehuisSet())
		{
			for (PlanningClient client : tehuis.getClientSet())
			{
				if (teSelecteren(client, context, uitnodigenTotEnMetJaar, null))
				{
					clientSet.add(client);
				}
			}
		}

		long totaalTotaal = 0L;
		long totaalVervolgRonde = 0L;
		long totaalEersteRonde = 0L;
		long totaalDubbeleTijd = 0L;
		long totaalMinderValide = 0L;
		long totaalTehuis = 0L;
		long totaalSuspect = 0L;
		long uitTeNodigenTotaal = 0L;
		long uitTeNodigenVervolgRonde = 0L;
		long uitTeNodigenEersteRonde = 0L;
		long uitTeNodigenDubbeleTijd = 0L;
		long uitTeNodigenMinderValide = 0L;
		long uitTeNodigenTehuis = 0L;
		long uitTeNodigenSuspect = 0L;
		for (PlanningClient client : clientSet.stream().filter(client -> !achtervangUitstelSet.contains(client) && !mindervalideUitwijkUitstelSet.contains(client))
			.collect(Collectors.toList()))
		{
			totaalTotaal++;
			if (uitTeNodigen(client))
			{
				uitTeNodigenTotaal++;
			}

			if (client.getVorigeScreeningRondeCreatieDatum() != null)
			{
				totaalVervolgRonde++;
				if (uitTeNodigen(client))
				{
					uitTeNodigenVervolgRonde++;
				}
			}
			else
			{
				totaalEersteRonde++;
				if (uitTeNodigen(client))
				{
					uitTeNodigenEersteRonde++;
				}
			}
			switch (client.getDoelgroep())
			{
			case DUBBELE_TIJD:
				totaalDubbeleTijd++;
				if (uitTeNodigen(client))
				{
					uitTeNodigenDubbeleTijd++;
				}
				break;
			case MINDER_VALIDE:
				totaalMinderValide++;
				if (uitTeNodigen(client))
				{
					uitTeNodigenMinderValide++;
				}
				break;
			}
			if (client.getTehuis() != null)
			{
				totaalTehuis++;
				if (uitTeNodigen(client))
				{
					uitTeNodigenTehuis++;
				}
			}
			if (client.isSuspect())
			{
				totaalSuspect++;
				if (uitTeNodigen(client))
				{
					uitTeNodigenSuspect++;
				}
			}
		}

		synchronized (rapportageDto)
		{
			PlanningStandplaatsRondeUitnodigenRapportageDto standplaatsRondeUitnodigenRapportage = rapportageDto.getStandplaatsRondeUitnodigenRapportages().stream()
				.filter(element -> element.getStandplaatsRondeId().equals(standplaatsRonde.getId())).findAny().orElse(null);

			if (context.uitnodigenOnderbreken)
			{
				standplaatsRondeUitnodigenRapportage.setStatus(MammaStandplaatsRondeRapportageStatus.ONDERBROKEN);
			}
			else
			{
				standplaatsRondeUitnodigenRapportage.setStatus(MammaStandplaatsRondeRapportageStatus.VOLTOOID);
			}

			standplaatsRondeUitnodigenRapportage.setTotaalTotaal(totaalTotaal);
			standplaatsRondeUitnodigenRapportage.setTotaalVervolgRonde(totaalVervolgRonde);
			standplaatsRondeUitnodigenRapportage.setTotaalEersteRonde(totaalEersteRonde);
			standplaatsRondeUitnodigenRapportage.setTotaalDubbeleTijd(totaalDubbeleTijd);
			standplaatsRondeUitnodigenRapportage.setTotaalMinderValide(totaalMinderValide);
			standplaatsRondeUitnodigenRapportage.setTotaalTehuis(totaalTehuis);
			standplaatsRondeUitnodigenRapportage.setTotaalSuspect(totaalSuspect);
			standplaatsRondeUitnodigenRapportage.setUitTeNodigenTotaal(uitTeNodigenTotaal);
			standplaatsRondeUitnodigenRapportage.setUitTeNodigenVervolgRonde(uitTeNodigenVervolgRonde);
			standplaatsRondeUitnodigenRapportage.setUitTeNodigenEersteRonde(uitTeNodigenEersteRonde);
			standplaatsRondeUitnodigenRapportage.setUitTeNodigenDubbeleTijd(uitTeNodigenDubbeleTijd);
			standplaatsRondeUitnodigenRapportage.setUitTeNodigenMinderValide(uitTeNodigenMinderValide);
			standplaatsRondeUitnodigenRapportage.setUitTeNodigenTehuis(uitTeNodigenTehuis);
			standplaatsRondeUitnodigenRapportage.setUitTeNodigenSuspect(uitTeNodigenSuspect);
		}
	}

	private static void scaleHuidigeStreefDatumEersteRondeClienten(NavigableSet<PlanningClient> clientNavigableSet)
	{
		List<PlanningClient> eClientList = new ArrayList<>(); 
		List<PlanningClient> vClientList = new ArrayList<>(); 

		for (PlanningClient client : clientNavigableSet)
		{
			if (client.getHuidigeStreefDatum() == null)
			{
				eClientList.add(client);
			}
			else
			{
				vClientList.add(client);
			}
		}

		if (eClientList.isEmpty())
		{

			return;
		}

		if (vClientList.isEmpty())
		{

			clientNavigableSet.removeAll(eClientList);
			for (PlanningClient client : eClientList)
			{
				client.setHuidigeStreefDatum(client.getGeboorteDatum().plusYears(PlanningConstanten.vanafLeeftijd));
			}
			clientNavigableSet.addAll(eClientList);
			return;
		}

		int eSize = eClientList.size();
		int eMinIndex = (int) Math.round(eSize / 20d);
		int eMaxIndex = eSize - 1 - eMinIndex;
		long eMin = eClientList.get(eMinIndex).getGeboorteDatum().toEpochDay();
		double eMax = eClientList.get(eMaxIndex).getGeboorteDatum().toEpochDay();
		double eInterval = eMax - eMin;

		int vSize = vClientList.size();
		int vMinIndex = (int) Math.round(vSize / 20d);
		int vMaxIndex = vSize - 1 - vMinIndex;
		long vMin = vClientList.get(vMinIndex).getHuidigeStreefDatum().toEpochDay();
		double vMax = vClientList.get(vMaxIndex).getHuidigeStreefDatum().toEpochDay();
		double vInterval = vMax - vMin;

		clientNavigableSet.removeAll(eClientList);

		if (eInterval != 0)
		{
			double ratio = vInterval / eInterval;

			for (PlanningClient client : eClientList)
			{
				long eX = client.getGeboorteDatum().toEpochDay() - eMin;
				long vX = Math.round(eX * ratio);
				client.setHuidigeStreefDatum(LocalDate.ofEpochDay(vMin + vX));
			}
		}
		else
		{
			for (PlanningClient client : eClientList)
			{
				client.setHuidigeStreefDatum(LocalDate.ofEpochDay(vMin));
			}
		}

		clientNavigableSet.addAll(eClientList);
	}
}
