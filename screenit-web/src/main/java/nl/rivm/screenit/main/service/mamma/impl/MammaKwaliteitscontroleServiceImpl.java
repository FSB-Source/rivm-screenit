package nl.rivm.screenit.main.service.mamma.impl;

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

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.NoSuchElementException;

import nl.rivm.screenit.Constants;
import nl.rivm.screenit.main.dao.mamma.MammaKwaliteitscontroleDao;
import nl.rivm.screenit.main.model.mamma.beoordeling.MammaAdhocMeekijkverzoekWerklijstZoekObject;
import nl.rivm.screenit.main.model.mamma.beoordeling.MammaFotobesprekingOnderzoekenWerklijstZoekObject;
import nl.rivm.screenit.main.model.mamma.beoordeling.MammaFotobesprekingWerklijstZoekObject;
import nl.rivm.screenit.main.model.mamma.beoordeling.MammaVisitatieOnderzoekenWerklijstZoekObject;
import nl.rivm.screenit.main.model.mamma.beoordeling.MammaVisitatieWerklijstZoekObject;
import nl.rivm.screenit.main.service.mamma.MammaKwaliteitscontroleService;
import nl.rivm.screenit.model.BeoordelingsEenheid;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.SortState;
import nl.rivm.screenit.model.UploadDocument;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.BezwaarType;
import nl.rivm.screenit.model.enums.FileStoreLocation;
import nl.rivm.screenit.model.mamma.MammaAdhocMeekijkverzoek;
import nl.rivm.screenit.model.mamma.MammaBeoordeling;
import nl.rivm.screenit.model.mamma.MammaFotobespreking;
import nl.rivm.screenit.model.mamma.MammaFotobesprekingOnderzoek;
import nl.rivm.screenit.model.mamma.MammaIKwaliteitscontrole;
import nl.rivm.screenit.model.mamma.MammaOnderzoek;
import nl.rivm.screenit.model.mamma.MammaScreeningRonde;
import nl.rivm.screenit.model.mamma.MammaScreeningsEenheid;
import nl.rivm.screenit.model.mamma.MammaVisitatie;
import nl.rivm.screenit.model.mamma.MammaVisitatieOnderzoek;
import nl.rivm.screenit.model.mamma.enums.MammaBeoordelingStatus;
import nl.rivm.screenit.model.mamma.enums.MammaFotobesprekingOnderzoekStatus;
import nl.rivm.screenit.model.mamma.enums.MammaMammografieIlmStatus;
import nl.rivm.screenit.model.mamma.enums.MammaVisitatieOnderdeel;
import nl.rivm.screenit.model.mamma.enums.MammaVisitatieOnderzoekStatus;
import nl.rivm.screenit.model.mamma.enums.MammaVisitatieStatus;
import nl.rivm.screenit.service.ClientService;
import nl.rivm.screenit.service.FileService;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.mamma.MammaBaseBeoordelingService;
import nl.rivm.screenit.service.mamma.MammaBaseOnderzoekService;
import nl.rivm.screenit.service.mamma.MammaBaseScreeningrondeService;
import nl.rivm.screenit.util.BezwaarUtil;
import nl.rivm.screenit.util.DateUtil;
import nl.rivm.screenit.util.mamma.MammaScreeningRondeUtil;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;

import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Service
@Transactional(propagation = Propagation.SUPPORTS, readOnly = true)
public class MammaKwaliteitscontroleServiceImpl implements MammaKwaliteitscontroleService
{
	private static final Logger LOG = LoggerFactory.getLogger(MammaKwaliteitscontroleServiceImpl.class);

	@Autowired
	private MammaKwaliteitscontroleDao kwaliteitscontroleDao;

	@Autowired
	private HibernateService hibernateService;

	@Autowired
	private ClientService clientService;

	@Autowired
	private MammaBaseOnderzoekService onderzoekService;

	@Autowired
	private ICurrentDateSupplier dateSuppier;

	@Autowired
	private MammaBaseScreeningrondeService baseScreeningrondeService;

	@Autowired
	private MammaBaseBeoordelingService baseBeoordelingService;

	@Autowired
	private FileService fileService;

	@Override
	public List<MammaFotobespreking> zoekFotobesprekingen(MammaFotobesprekingWerklijstZoekObject zoekObject, int first, int count, String sortProperty, boolean asc)
	{
		return kwaliteitscontroleDao.zoekFotobesprekingen(zoekObject, first, count, sortProperty, asc);
	}

	@Override
	public long countFotobesprekingen(MammaFotobesprekingWerklijstZoekObject zoekObject)
	{
		return kwaliteitscontroleDao.countFotobesprekingen(zoekObject);
	}

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public List<String> saveOrUpdateFotobespreking(MammaFotobespreking fotobespreking, File file)
	{
		hibernateService.saveOrUpdate(fotobespreking);

		List<String> meldingen = new ArrayList<>();
		if (file != null)
		{
			if (!fotobespreking.getOnderzoeken().isEmpty())
			{
				hibernateService.deleteAll(fotobespreking.getOnderzoeken());
				fotobespreking.getOnderzoeken().clear();
				hibernateService.saveOrUpdate(fotobespreking);
			}
			ClientenBestandVerwerkingContext context = null;
			try
			{

				context = new ClientenBestandVerwerkingContext(file);
				while (context.isErEenNieuweRegel())
				{
					try
					{
						String melding = verwerkRegel(fotobespreking, context);
						if (StringUtils.isNotBlank(melding))
						{
							meldingen.add(melding);
						}
					}
					catch (IllegalStateException ise)
					{
						meldingen.add(ise.getMessage());
					}
				}
				hibernateService.saveOrUpdate(fotobespreking);
			}
			catch (Exception e)
			{
				String errorMessage = e instanceof IllegalStateException ? e.getMessage() : "Onbekende fout bij verwerking van clientenlijst bestand.";
				meldingen.add(errorMessage);
				if (!(e instanceof IllegalStateException))
				{
					LOG.error(errorMessage, e);
				}
			}
			finally
			{
				if (context != null)
				{
					context.close();
				}
			}
		}
		return meldingen;
	}

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public String addFotobesprekingOnderzoek(MammaFotobespreking fotobespreking, Client client)
	{
		String melding = null;
		try
		{
			MammaBeoordeling beoordeling = getBeoordeling(fotobespreking.getBeoordelingsEenheid(), fotobespreking.getScreeningsEenheid(), client);
			if (!kwaliteitscontroleDao.isBeoordelingInBespreking(beoordeling, fotobespreking))
			{
				MammaFotobesprekingOnderzoek fotobesprekingOnderzoek = new MammaFotobesprekingOnderzoek();
				List<MammaFotobesprekingOnderzoek> onderzoeken = fotobespreking.getOnderzoeken();
				fotobesprekingOnderzoek.setFotobespreking(fotobespreking);
				fotobesprekingOnderzoek.setBeoordeling(beoordeling);
				fotobesprekingOnderzoek.setStatus(MammaFotobesprekingOnderzoekStatus.NIET_BESPROKEN);
				int volgnummer = onderzoeken.isEmpty() ? 1
					: onderzoeken.stream().mapToInt(MammaFotobesprekingOnderzoek::getVolgnummer).max().orElseThrow(NoSuchElementException::new) + 1;
				fotobesprekingOnderzoek.setVolgnummer(volgnummer);
				onderzoeken.add(fotobesprekingOnderzoek);
				hibernateService.saveOrUpdateAll(fotobesprekingOnderzoek, fotobespreking);
			}
			else
			{
				throw new IllegalStateException(String.format("Onderzoek van cliënt met bsn %s en geboortedatum %s kan niet worden toegevoegd: Onderzoek staat al in de werklijst",
					client.getPersoon().getBsn(), DateUtil.getGeboortedatum(client)));
			}
		}
		catch (Exception e)
		{
			melding = e instanceof IllegalStateException ? e.getMessage() : "Onbekende fout.";
			if (!(e instanceof IllegalStateException))
			{
				LOG.error(melding, e);
			}
		}
		return melding;
	}

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public void deleteBesprokenFotobesprekingOnderzoek(MammaFotobesprekingOnderzoek fotobesprekingOnderzoek)
	{
		if (fotobesprekingOnderzoek.getStatus() == MammaFotobesprekingOnderzoekStatus.NIET_BESPROKEN)
		{
			MammaFotobespreking fotobespreking = fotobesprekingOnderzoek.getFotobespreking();
			fotobespreking.getOnderzoeken().remove(fotobesprekingOnderzoek);
			hibernateService.delete(fotobesprekingOnderzoek);
			hibernateService.saveOrUpdate(fotobespreking);
		}
		else
		{
			throw new IllegalStateException("Onderzoek is al besproken");
		}
	}

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public void deleteFotobespreking(MammaFotobespreking fotobespreking)
	{
		hibernateService.delete(fotobespreking);
	}

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public void deleteVisitatieOnderzoek(MammaVisitatieOnderzoek visitatieOnderzoek)
	{
		if (visitatieOnderzoek.getStatus() == MammaVisitatieOnderzoekStatus.NIET_GEZIEN)
		{
			MammaVisitatie visitatie = visitatieOnderzoek.getVisitatie();
			visitatie.getOnderzoeken().remove(visitatieOnderzoek);
			hibernateService.delete(visitatieOnderzoek);
			hibernateService.saveOrUpdate(visitatie);
		}
		else
		{
			throw new IllegalStateException("Onderzoek is al gezien.");
		}
	}

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public String addVisitatieOnderzoek(MammaVisitatie visitatie, MammaVisitatieOnderdeel visitatieOnderdeel, Client client)
	{
		String melding = null;
		try
		{
			if (!BezwaarUtil.isBezwaarActiefVoor(client, BezwaarType.GEEN_WETENSCHAPPELIJK_ONDERZOEK, Bevolkingsonderzoek.MAMMA)
				&& !BezwaarUtil.isBezwaarActiefVoor(client, BezwaarType.GEEN_KWALITEITSWAARBORGING, Bevolkingsonderzoek.MAMMA))
			{
				MammaBeoordeling beoordeling = getBeoordeling(visitatie.getBeoordelingsEenheid(), null, client);
				if (!kwaliteitscontroleDao.isBeoordelingInVisitatieOnderdeel(beoordeling, visitatie, visitatieOnderdeel))
				{
					List<MammaVisitatieOnderzoek> onderzoeken = visitatie.getOnderzoeken();
					MammaVisitatieOnderzoek visitatieOnderzoek = new MammaVisitatieOnderzoek();
					visitatieOnderzoek.setVisitatie(visitatie);
					visitatieOnderzoek.setBeoordeling(beoordeling);
					visitatieOnderzoek.setStatus(MammaVisitatieOnderzoekStatus.NIET_GEZIEN);
					visitatieOnderzoek.setOnderdeel(visitatieOnderdeel);
					int volgnummer = onderzoeken.stream().filter(o -> o.getOnderdeel() == visitatieOnderdeel).mapToInt(MammaVisitatieOnderzoek::getVolgnummer).max().orElse(0) + 1;
					visitatieOnderzoek.setVolgnummer(volgnummer);
					onderzoeken.add(visitatieOnderzoek);
					hibernateService.saveOrUpdateAll(visitatieOnderzoek, visitatie);
				}
				else
				{
					throw new IllegalStateException(
						String.format("Onderzoek van cliënt met bsn %s en geboortedatum %s kan niet worden toegevoegd: Onderzoek staat al in de werklijst",
							client.getPersoon().getBsn(), DateUtil.getGeboortedatum(client)));
				}
			}
			else
			{
				throw new IllegalStateException(
					String.format("Onderzoek van cliënt met bsn %s en geboortedatum %s kan niet worden toegevoegd: Cliënt heeft actief bezwaar",
						client.getPersoon().getBsn(), DateUtil.getGeboortedatum(client)));
			}
		}
		catch (Exception e)
		{
			melding = e instanceof IllegalStateException ? e.getMessage() : "Onbekende fout.";
			if (!(e instanceof IllegalStateException))
			{
				LOG.error(melding, e);
			}
		}
		return melding;
	}

	private String verwerkRegel(MammaFotobespreking fotobespreking, ClientenBestandVerwerkingContext context)
	{
		Client client = getCsvClient(context);
		return addFotobesprekingOnderzoek(fotobespreking, client);
	}

	private Client getCsvClient(ClientenBestandVerwerkingContext context)
	{
		String bsn = context.getBsnVanHuidigeRegel();
		Date geboortedatum = context.getGeboortedatumVanHuidigeRegel();
		Client client = clientService.getClientByBsn(context.getBsnVanHuidigeRegel());
		if (client == null || !geboortedatum.equals(client.getPersoon().getGeboortedatum()))
		{
			throw new IllegalStateException("Client met bsn " + bsn + " en geboortedatum " + Constants.getDateFormat().format(geboortedatum) + ": " + "niet gevonden.");
		}
		else
		{
			return client;
		}
	}

	private MammaBeoordeling getBeoordeling(BeoordelingsEenheid beoordelingsEenheid, MammaScreeningsEenheid screeningsEenheid, Client client)
	{
		String foutPrefix = "Client met bsn " + client.getPersoon().getBsn() + " en geboortedatum " + DateUtil.getGeboortedatum(client)
			+ ": ";
		MammaScreeningRonde screeningRonde = baseScreeningrondeService.getLaatsteScreeningRondeMetUitslag(client);

		MammaOnderzoek onderzoek = MammaScreeningRondeUtil.getLaatsteOnderzoek(screeningRonde);
		if (onderzoek == null)
		{
			throw new IllegalStateException(foutPrefix + "geen onderzoek gevonden.");
		}
		if (onderzoek.getMammografie().getIlmStatus() != MammaMammografieIlmStatus.BESCHIKBAAR)
		{
			throw new IllegalStateException(foutPrefix + "geen beelden beschikbaar");
		}
		MammaBeoordeling beoordeling = onderzoek.getLaatsteBeoordeling();
		if (beoordeling == null || !MammaBeoordelingStatus.isUitslagStatus(beoordeling.getStatus()))
		{
			String foutMelding = foutPrefix + "beoordeling van onderzoek is nog niet afgerond.";
			LOG.error("Client met id " + client.getId() + ": Status " + (beoordeling != null ? beoordeling.getStatus() : "geen"));
			throw new IllegalStateException(foutMelding);
		}
		if (beoordelingsEenheid != null && !beoordeling.getBeoordelingsEenheid().equals(beoordelingsEenheid))
		{
			throw new IllegalStateException(foutPrefix + "beoordeling is niet doorgevoerd in opgegeven beoordelingseenheid.");
		}
		if (screeningsEenheid != null && !onderzoek.getScreeningsEenheid().equals(screeningsEenheid))
		{
			throw new IllegalStateException(foutPrefix + "onderzoek is niet doorgevoerd in opgegeven screeningseenheid.");
		}
		return beoordeling;
	}

	@Override
	public List<MammaFotobesprekingOnderzoek> zoekFotobesprekingOnderzoeken(MammaFotobesprekingOnderzoekenWerklijstZoekObject zoekObject, int first, int count,
		String sortProperty, boolean ascending)
	{
		return kwaliteitscontroleDao.zoekFotobesprekingOnderzoeken(zoekObject, first, count, sortProperty, ascending);
	}

	@Override
	public long countFotobesprekingOnderzoeken(MammaFotobesprekingOnderzoekenWerklijstZoekObject zoekObject)
	{
		return kwaliteitscontroleDao.countFotobesprekingOnderzoeken(zoekObject);
	}

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public void wijzigOnderzoekStatus(MammaFotobesprekingOnderzoek fotobesprekingOnderzoek, MammaFotobesprekingOnderzoekStatus nieuweStatus)
	{
		fotobesprekingOnderzoek.setStatus(nieuweStatus);
		hibernateService.saveOrUpdate(fotobesprekingOnderzoek);
	}

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public void startKwaliteitscontrole(MammaIKwaliteitscontrole kwaliteitscontrole)
	{
		kwaliteitscontrole.setGestartOp(dateSuppier.getDate());
		hibernateService.saveOrUpdate(kwaliteitscontrole);
	}

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public void fotobesprekingAfronden(MammaFotobespreking fotobespreking)
	{
		kwaliteitscontroleAfronden(fotobespreking);
	}

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public void herbeoordeelFotobesprekingOnderzoek(MammaFotobesprekingOnderzoek fotobesprekingOnderzoek)
	{
		MammaBeoordeling beoordeling = fotobesprekingOnderzoek.getBeoordeling();
		if (baseBeoordelingService.isUitslagGunstig(beoordeling))
		{
			if (beoordeling.getOnderzoek().getLaatsteBeoordeling().equals(beoordeling))
			{
				MammaScreeningRonde screeningRonde = beoordeling.getOnderzoek().getAfspraak().getUitnodiging().getScreeningRonde();
				if (screeningRonde.equals(screeningRonde.getDossier().getLaatsteScreeningRonde()))
				{
					onderzoekService.voegNieuweBeoordelingToe(beoordeling.getOnderzoek());
					fotobesprekingOnderzoek.setStatus(MammaFotobesprekingOnderzoekStatus.NIEUWE_BEOORDELING_AANGEMAAKT);
					hibernateService.saveOrUpdate(fotobesprekingOnderzoek);
				}
				else
				{
					throw new IllegalStateException("er is al een nieuwe ronde gestart voor deze cliënt");
				}
			}
			else
			{
				throw new IllegalStateException("er is al een herbeoordeling aangemaakt voor dit onderzoek.");
			}
		}
		else
		{
			throw new IllegalStateException("uitslag van beoordeling is niet gunstig");
		}
	}

	@Override
	public boolean kanFotobesprekingAfronden(MammaFotobespreking fotobespreking)
	{
		return kwaliteitscontroleDao.isAllesBesproken(fotobespreking) && fotobespreking.getAfgerondOp() == null;
	}

	@Override
	public Integer getAantalBesproken(MammaFotobespreking fotobespreking)
	{
		return kwaliteitscontroleDao.getAantalBesproken(fotobespreking);
	}

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public void wijzigOnderzoekStatus(MammaVisitatieOnderzoek visitatieOnderzoek, MammaVisitatieOnderzoekStatus nieuweOnderzoekStatus)
	{
		visitatieOnderzoek.setStatus(nieuweOnderzoekStatus);
		hibernateService.saveOrUpdate(visitatieOnderzoek);
	}

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public List<String> saveOrUpdateVisitatie(MammaVisitatie visitatie, Map<MammaVisitatieOnderdeel, File> fileMap, Map<File, String> fileNameMap,
		UploadDocument rapportageBijlage, UploadDocument vragenlijstBijlage)
	{
		List<String> messages = new ArrayList<>();
		hibernateService.saveOrUpdate(visitatie);
		messages.addAll(saveOrUpdateVisitatieBijlagen(visitatie, rapportageBijlage, vragenlijstBijlage));
		messages.addAll(verwerkCsvFiles(visitatie, fileMap, fileNameMap));
		hibernateService.saveOrUpdate(visitatie);
		return messages;
	}

	private List<String> saveOrUpdateVisitatieBijlagen(MammaVisitatie visitatie, UploadDocument rapportageBijlage, UploadDocument vragenlijstBijlage)
	{
		List<String> messages = new ArrayList<>();
		if (rapportageBijlage != null)
		{
			messages.addAll(saveOrUpdateVisitatieBijlage(visitatie.getId(), visitatie.getRapportageBijlage(), rapportageBijlage));
			visitatie.setRapportageBijlage(rapportageBijlage);
		}
		if (vragenlijstBijlage != null)
		{
			messages.addAll(saveOrUpdateVisitatieBijlage(visitatie.getId(), visitatie.getVragenlijstBijlage(), vragenlijstBijlage));
			visitatie.setVragenlijstBijlage(vragenlijstBijlage);
		}

		return messages;
	}

	private List<String> verwerkCsvFiles(MammaVisitatie visitatie, Map<MammaVisitatieOnderdeel, File> fileMap, Map<File, String> fileNameMap)
	{
		List<String> meldingen = new ArrayList<>();
		if (fileMap.size() > 0 && !visitatie.getOnderzoeken().isEmpty())
		{
			hibernateService.deleteAll(visitatie.getOnderzoeken());
			visitatie.getOnderzoeken().clear();
			hibernateService.saveOrUpdate(visitatie);
		}

		for (Entry<MammaVisitatieOnderdeel, File> fileEntry : fileMap.entrySet())
		{
			File file = fileEntry.getValue();
			String fileName = fileNameMap.get(file);
			if (file != null)
			{
				ClientenBestandVerwerkingContext context = null;
				try
				{
					context = new ClientenBestandVerwerkingContext(file);
					while (context.isErEenNieuweRegel())
					{
						try
						{
							String melding = verwerkRegel(visitatie, fileEntry.getKey(), context);
							if (StringUtils.isNotBlank(melding))
							{
								meldingen.add(String.format("%1$s (Bestand: %2$s)", melding, fileName));
							}
						}
						catch (IllegalStateException ise)
						{
							meldingen.add(String.format("%1$s (Bestand: %2$s)", ise.getMessage(), fileName));
						}
					}
					hibernateService.saveOrUpdate(visitatie);
				}
				catch (Exception e)
				{
					String melding = e instanceof IllegalStateException ? e.getMessage() : "Onbekende fout bij verwerking van clientenlijst bestand.";
					meldingen.add(String.format("%1$s (Bestand: %2$s)", melding, fileName));
					if (!(e instanceof IllegalStateException))
					{
						LOG.error(melding, e);
					}
				}
				finally
				{
					if (context != null)
					{
						context.close();
					}
				}
			}
		}
		return meldingen;
	}

	private List<String> saveOrUpdateVisitatieBijlage(long id, UploadDocument oudeBijlage, UploadDocument bijlage)
	{
		List<String> messages = new ArrayList<>();
		if (oudeBijlage != null && !oudeBijlage.equals(bijlage))
		{
			oudeBijlage.setActief(false);
			hibernateService.saveOrUpdate(oudeBijlage);
		}
		try
		{
			fileService.saveOrUpdateUploadDocument(bijlage, FileStoreLocation.MAMMA_VISITATIE_BIJLAGE, id, true);
		}
		catch (IllegalStateException | IOException e)
		{
			String errorMessage = "Onbekende fout bij verwerking van bijlagen.";
			LOG.error(errorMessage, e);
			messages.add(errorMessage);
		}
		return messages;
	}

	private String verwerkRegel(MammaVisitatie visitatie, MammaVisitatieOnderdeel onderdeel, ClientenBestandVerwerkingContext context)
	{
		Client client = getCsvClient(context);
		return addVisitatieOnderzoek(visitatie, onderdeel, client);
	}

	@Override
	public List<MammaVisitatieOnderzoek> zoekVisitatieOnderzoeken(MammaVisitatieOnderzoekenWerklijstZoekObject zoekObject, int first, int count, String sortProperty,
		boolean ascending)
	{
		return kwaliteitscontroleDao.zoekVisitatieOnderzoeken(zoekObject, first, count, sortProperty, ascending);
	}

	@Override
	public long countVisitatieOnderzoeken(MammaVisitatieOnderzoekenWerklijstZoekObject zoekObject)
	{
		return kwaliteitscontroleDao.countVisitatieOnderzoeken(zoekObject);
	}

	@Override
	public List<MammaVisitatie> zoekVisitaties(MammaVisitatieWerklijstZoekObject zoekObject, int first, int count, String sortProperty, boolean asc)
	{
		return kwaliteitscontroleDao.zoekVisitaties(zoekObject, first, count, sortProperty, asc);
	}

	@Override
	public long countVisitaties(MammaVisitatieWerklijstZoekObject zoekObject)
	{
		return kwaliteitscontroleDao.countVisitaties(zoekObject);
	}

	@Override
	public Integer getAantalGezien(MammaVisitatie visitatie, MammaVisitatieOnderdeel onderdeel)
	{
		return kwaliteitscontroleDao.getAantalGezien(visitatie, onderdeel);
	}

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public void kwaliteitscontroleAfronden(MammaIKwaliteitscontrole kwaliteitscontrole)
	{
		kwaliteitscontrole.setAfgerondOp(dateSuppier.getDate());
		hibernateService.saveOrUpdate(kwaliteitscontrole);
	}

	@Override
	public boolean kanVisitatieAfronden(MammaVisitatie visitatie)
	{
		return kwaliteitscontroleDao.isAllesGezien(visitatie) && visitatie.getAfgerondOp() == null;
	}

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public void visitatieAfronden(MammaVisitatie visitatie)
	{
		visitatie.setStatus(MammaVisitatieStatus.UITGEVOERD);
		kwaliteitscontroleAfronden(visitatie);
	}

	@Override
	public boolean nieuweBeoordelingenAangevraagdNavFotobespreking(MammaFotobespreking fotobespreking)
	{
		return kwaliteitscontroleDao.nieuweBeoordelingenAangevraagdNavFotobespreking(fotobespreking);
	}

	@Override
	public List<MammaAdhocMeekijkverzoek> zoekAdhocMeekijkverzoekOnderzoeken(MammaAdhocMeekijkverzoekWerklijstZoekObject zoekObject, int first, int count,
		SortState<String> sortState)
	{
		return kwaliteitscontroleDao.zoekAdhocMeekijkverzoekOnderzoeken(zoekObject, first, count, sortState);
	}

	@Override
	public long countAdhocMeekijkverzoekOnderzoeken(MammaAdhocMeekijkverzoekWerklijstZoekObject zoekObject)
	{
		return kwaliteitscontroleDao.countAdhocMeekijkverzoekOnderzoeken(zoekObject);
	}

	@Override
	public Integer getAantalGezienAdhocMeekijkverzoekOnderzoeken(MammaAdhocMeekijkverzoekWerklijstZoekObject zoekObject)
	{
		return kwaliteitscontroleDao.getAantalGezienAdhocMeekijkverzoekOnderzoeken(zoekObject);
	}

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public void wijzigOnderzoekStatus(MammaAdhocMeekijkverzoek onderzoek, MammaVisitatieOnderzoekStatus nieuweStatus)
	{
		onderzoek.setStatus(nieuweStatus);
		hibernateService.saveOrUpdate(onderzoek);
	}

	@Override
	public Integer getAantalGezienAdhocMeekijkverzoekOnderzoekenInList(List<Long> onderzoekenIds)
	{
		return kwaliteitscontroleDao.getAantalGezienAdhocMeekijkverzoekOnderzoekenInList(onderzoekenIds);
	}

}
