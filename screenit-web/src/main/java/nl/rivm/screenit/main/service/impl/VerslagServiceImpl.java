
package nl.rivm.screenit.main.service.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
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

import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Modifier;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

import nl.rivm.screenit.Constants;
import nl.rivm.screenit.dao.VerslagDao;
import nl.rivm.screenit.dao.mamma.MammaBaseBeoordelingDao;
import nl.rivm.screenit.main.service.FormulierService;
import nl.rivm.screenit.main.service.VerslagService;
import nl.rivm.screenit.model.BerichtZoekFilter;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.DossierStatus;
import nl.rivm.screenit.model.Functie;
import nl.rivm.screenit.model.Gebruiker;
import nl.rivm.screenit.model.InstellingGebruiker;
import nl.rivm.screenit.model.ScreeningRonde;
import nl.rivm.screenit.model.ScreeningRondeStatus;
import nl.rivm.screenit.model.berichten.Verslag;
import nl.rivm.screenit.model.berichten.cda.OntvangenCdaBericht;
import nl.rivm.screenit.model.berichten.enums.BerichtType;
import nl.rivm.screenit.model.berichten.enums.VerslagStatus;
import nl.rivm.screenit.model.berichten.enums.VerslagType;
import nl.rivm.screenit.model.colon.ColonDossier;
import nl.rivm.screenit.model.colon.ColonIntakeAfspraak;
import nl.rivm.screenit.model.colon.ColonScreeningRonde;
import nl.rivm.screenit.model.colon.ColonVerslag;
import nl.rivm.screenit.model.colon.MdlVerslag;
import nl.rivm.screenit.model.colon.PaVerslag;
import nl.rivm.screenit.model.colon.enums.MdlVervolgbeleid;
import nl.rivm.screenit.model.colon.verslag.mdl.MdlIncidentcomplicatie;
import nl.rivm.screenit.model.colon.verslag.mdl.MdlLaesiecoloscopiecentrum;
import nl.rivm.screenit.model.colon.verslag.mdl.MdlMedicatie;
import nl.rivm.screenit.model.colon.verslag.mdl.MdlMedicatiemiddel;
import nl.rivm.screenit.model.colon.verslag.mdl.MdlVerslagContent;
import nl.rivm.screenit.model.colon.verslag.pa.PaPathologieProtocolColonbioptperPoliep;
import nl.rivm.screenit.model.colon.verslag.pa.PaVerslagContent;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.model.formulieren.GebruikerAntwoord;
import nl.rivm.screenit.model.formulieren.IdentifierElement;
import nl.rivm.screenit.model.formulieren.PalgaNumber;
import nl.rivm.screenit.model.formulieren.PalgaNumberAntwoord;
import nl.rivm.screenit.model.mamma.MammaDossier;
import nl.rivm.screenit.model.mamma.MammaFollowUpVerslag;
import nl.rivm.screenit.model.mamma.MammaScreeningRonde;
import nl.rivm.screenit.model.verslag.DSValue;
import nl.rivm.screenit.model.verslag.VerslagContent;
import nl.rivm.screenit.service.BerichtToBatchService;
import nl.rivm.screenit.service.LogService;
import nl.rivm.screenit.service.VerwerkVerslagService;
import nl.rivm.screenit.service.colon.ColonDossierBaseService;
import nl.rivm.screenit.service.mamma.MammaBaseFollowUpService;
import nl.rivm.screenit.util.DateUtil;
import nl.rivm.screenit.util.RomanNumeral;
import nl.topicuszorg.formulieren2.api.definitie.VraagDefinitie;
import nl.topicuszorg.formulieren2.api.resultaat.Antwoord;
import nl.topicuszorg.formulieren2.api.resultaat.FormulierResultaat;
import nl.topicuszorg.formulieren2.persistence.instantie.VerplichtingImpl;
import nl.topicuszorg.formulieren2.persistence.instantie.VraagInstantieImpl;
import nl.topicuszorg.formulieren2.persistence.resultaat.BooleanAntwoord;
import nl.topicuszorg.formulieren2.persistence.resultaat.DateAntwoord;
import nl.topicuszorg.formulieren2.persistence.resultaat.FormulierResultaatImpl;
import nl.topicuszorg.formulieren2.persistence.resultaat.StringAntwoord;
import nl.topicuszorg.hibernate.object.helper.HibernateHelper;
import nl.topicuszorg.hibernate.object.model.HibernateObject;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.wicket.hibernate.cglib.ModelProxyHelper;

import org.apache.commons.beanutils.PropertyUtils;
import org.apache.commons.lang.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Service
@Transactional(propagation = Propagation.SUPPORTS, readOnly = true)
public class VerslagServiceImpl implements VerslagService
{

	private static final String VRAAG_SEDATIE_JA_NEE = "sedatie_ja_nee_vraag";

	private static final String PATIENTNUMMER = "patientnummer";

	private static final String VRAAG_AANTAL_POTJES = "aantal_potjes";

	private static final String VRAAG_PATHOLOOG = "patholoog_vraag";

	private static final String VRAAG_ENDOSCOPIST = "endoscopist_vraag";

	private static final Logger LOG = LoggerFactory.getLogger(VerslagServiceImpl.class);

	@Autowired
	private HibernateService hibernateService;

	@Autowired
	private VerwerkVerslagService verwerkVerslagService;

	@Autowired
	private VerslagDao verslagDao;

	@Autowired
	private FormulierService formulierService;

	@Autowired
	private LogService logService;

	@Autowired
	private BerichtToBatchService cdaBerichtToBatchService;

	@Autowired(required = false)
	private ColonDossierBaseService colonDossierService;

	@Autowired(required = false)
	private MammaBaseBeoordelingDao mammaBeoordelingDao;

	@Autowired(required = false)
	private MammaBaseFollowUpService followUpService;

	private ExecutorService executorService;

	public VerslagServiceImpl()
	{
		executorService = Executors.newSingleThreadExecutor();
	}

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public void saveOrAfronden(VerslagContent<?> verslagContent, FormulierResultaat resultaat, boolean afronden, InstellingGebruiker instellingGebruiker)
	{
		try
		{
			correctValues(verslagContent, null);
		}
		catch (IllegalAccessException | InvocationTargetException | NoSuchMethodException e)
		{
			LOG.error("Error in correctValues: ", e);
		}

		Verslag verslag = verslagContent.getVerslag();

		for (Antwoord<?> antwoord : resultaat.getAntwoorden())
		{
			VraagDefinitie<?> vraagDefinitie = antwoord.getVraagInstantie().getVraagDefinitie();
			if (vraagDefinitie instanceof IdentifierElement)
			{
				IdentifierElement identifierElement = (IdentifierElement) vraagDefinitie;
				if (identifierElement.getIdentifier() != null)
				{
					switch (identifierElement.getIdentifier())
					{
					case VRAAG_PATHOLOOG:
					case VRAAG_ENDOSCOPIST:
						if (antwoord instanceof GebruikerAntwoord)
						{
							GebruikerAntwoord gebruikerAntwoord = (GebruikerAntwoord) antwoord;
							Gebruiker gebruiker = gebruikerAntwoord.getValue();
							if (gebruiker != null)
							{
								verslag.setUitvoerderMedewerker(gebruiker);
								verslag.setUitvoerderOrganisatie(verslag.getInvoerder().getOrganisatie());
							}
						}
						break;
					case VRAAG_AANTAL_POTJES:
						if (antwoord instanceof PalgaNumberAntwoord)
						{
							PalgaNumberAntwoord palgaNumberAntwoord = (PalgaNumberAntwoord) antwoord;
							PalgaNumber palgaNumber = palgaNumberAntwoord.getValue();
							Integer waarde = Integer.valueOf(0);
							int start = 0;
							if (palgaNumber != null)
							{
								start = 1;
								if (palgaNumber.getStringValue().startsWith("XI"))
								{
									start = 11;
								}
								else if (palgaNumber.getStringValue().startsWith("VI")) 
								{
									start = 6;
								}
								waarde = palgaNumber.getWaarde();
							}
							PaVerslagContent paVerslagContent = (PaVerslagContent) verslagContent;

							List<PaPathologieProtocolColonbioptperPoliep> potjesToDelete = new ArrayList<>();
							for (PaPathologieProtocolColonbioptperPoliep potje : paVerslagContent.getPathologieProtocolColonbioptperPoliep())
							{
								if (waarde > 0)
								{
									potje.setNummerPotjeMateriaal("" + start++);
									waarde--;
								}
								else
								{
									potjesToDelete.add(potje);
								}
							}
							for (PaPathologieProtocolColonbioptperPoliep potjeToDelete : potjesToDelete)
							{
								potjeToDelete.setVerslagContent(null);
								paVerslagContent.getPathologieProtocolColonbioptperPoliep().remove(potjeToDelete);
								hibernateService.delete(potjeToDelete);
							}
						}
						break;
					case Constants.VRAAG_DATUM_VERRICHTING:
						if (antwoord instanceof DateAntwoord)
						{
							DateAntwoord dateAntwoord = (DateAntwoord) antwoord;
							Date value = dateAntwoord.getValue();
							if (value != null)
							{
								MdlVerslagContent mdlVerslagContent = (MdlVerslagContent) verslagContent;
								mdlVerslagContent.getVerrichting().setAanvangVerrichting(value);
								verslag.setDatumOnderzoek(value);
							}
						}
						break;
					case VRAAG_SEDATIE_JA_NEE:
						if (antwoord instanceof BooleanAntwoord)
						{
							BooleanAntwoord booleanAntwoord = (BooleanAntwoord) antwoord;
							Boolean value = booleanAntwoord.getValue();
							if (!Boolean.TRUE.equals(value))
							{
								MdlVerslagContent mdlVerslagContent = (MdlVerslagContent) verslagContent;

								if (mdlVerslagContent.getColoscopieMedischeObservatie() != null && mdlVerslagContent.getColoscopieMedischeObservatie().getMedicatie() != null)
								{
									MdlMedicatie medicatie = mdlVerslagContent.getColoscopieMedischeObservatie().getMedicatie();
									if (medicatie.getMedicatiemiddel() != null)
									{
										for (MdlMedicatiemiddel medicatiemiddel : medicatie.getMedicatiemiddel())
										{
											hibernateService.delete(medicatiemiddel);
										}
										medicatie.setMedicatiemiddel(new ArrayList<MdlMedicatiemiddel>());
									}
									medicatie.setMateVanSedatie(null);
								}
							}
						}
						break;
					case Constants.VRAAG_LAESIE_JA_NEE:
						if (antwoord instanceof BooleanAntwoord)
						{
							BooleanAntwoord booleanAntwoord = (BooleanAntwoord) antwoord;
							Boolean value = booleanAntwoord.getValue();
							if (!Boolean.TRUE.equals(value))
							{
								MdlVerslagContent mdlVerslagContent = (MdlVerslagContent) verslagContent;
								if (mdlVerslagContent.getLaesiecoloscopiecentrum() != null)
								{
									for (MdlLaesiecoloscopiecentrum laesie : mdlVerslagContent.getLaesiecoloscopiecentrum())
									{
										hibernateService.delete(laesie);
									}
									mdlVerslagContent.setLaesiecoloscopiecentrum(new ArrayList<MdlLaesiecoloscopiecentrum>());
								}
							}
						}
						break;
					case Constants.VRAAG_INCIDENT_COMPLICATIE_JA_NEE:
						if (antwoord instanceof BooleanAntwoord)
						{
							BooleanAntwoord booleanAntwoord = (BooleanAntwoord) antwoord;
							Boolean value = booleanAntwoord.getValue();
							if (!Boolean.TRUE.equals(value))
							{
								MdlVerslagContent mdlVerslagContent = (MdlVerslagContent) verslagContent;
								if (mdlVerslagContent.getVerrichting() != null && mdlVerslagContent.getVerrichting().getIncidentcomplicatie() != null)
								{
									for (MdlIncidentcomplicatie incidentcomplicatie : mdlVerslagContent.getVerrichting().getIncidentcomplicatie())
									{
										hibernateService.delete(incidentcomplicatie);
									}
									mdlVerslagContent.getVerrichting().setIncidentcomplicatie(new ArrayList<MdlIncidentcomplicatie>());
								}
							}
						}
						break;
					case PATIENTNUMMER:
						if (antwoord instanceof StringAntwoord)
						{
							StringAntwoord stringAntwoord = (StringAntwoord) antwoord;
							String value = stringAntwoord.getValue();
							MdlVerslag mdlVerslag = (MdlVerslag) verslag;
							mdlVerslag.setPatientnummer(value);
						}
						break;
					default:
						break;
					}
				}
			}
		}
		logAction(verslag, afronden, instellingGebruiker);
		if (afronden)
		{
			verslag.setStatus(VerslagStatus.AFGEROND);
		}
		hibernateService.saveOrUpdate(verslagContent.getVerslag());
		hibernateService.saveOrUpdate(verslagContent);
		if (afronden)
		{
			verslag.setStatus(VerslagStatus.AFGEROND);
			verwerkVerslagService.onAfterVerwerkVerslagContent(verslag);
			verwerkVerslagService.verwerkInDossier(verslag);
		}
	}

	@SuppressWarnings("rawtypes")
	private void logAction(Verslag verslag, boolean afgerond, InstellingGebruiker instellingGebruiker)
	{
		Client client = verslag.getScreeningRonde().getDossier().getClient();
		String createMelding = createLogMelding(verslag);
		if (verslag.getType() == VerslagType.MDL)
		{
			if (afgerond)
			{
				logService.logGebeurtenis(LogGebeurtenis.MDL_VERSLAG_VAST, instellingGebruiker, client, createMelding, Bevolkingsonderzoek.COLON);
			}
			else
			{
				logService.logGebeurtenis(LogGebeurtenis.MDL_VERSLAG_WIJZIG, instellingGebruiker, client, createMelding, Bevolkingsonderzoek.COLON);
			}
		}
		else if (verslag.getType() == VerslagType.PA_LAB)
		{
			if (afgerond)
			{
				logService.logGebeurtenis(LogGebeurtenis.PA_VERSLAG_VAST, instellingGebruiker, client, createMelding, Bevolkingsonderzoek.COLON);
			}
			else
			{
				logService.logGebeurtenis(LogGebeurtenis.PA_VERSLAG_WIJZIG, instellingGebruiker, client, createMelding, Bevolkingsonderzoek.COLON);
			}
		}
		else if (verslag.getType() == VerslagType.MAMMA_PA_FOLLOW_UP)
		{
			if (afgerond)
			{
				logService.logGebeurtenis(LogGebeurtenis.MAMMA_VERSLAG_FOLLOW_UP_VAST, instellingGebruiker, client, createMelding, Bevolkingsonderzoek.MAMMA);
			}
			else
			{
				logService.logGebeurtenis(LogGebeurtenis.MAMMA_VERSLAG_FOLLOW_UP_WIJZIG, instellingGebruiker, client, createMelding, Bevolkingsonderzoek.MAMMA);
			}
		}
	}

	private void correctValues(Object object, Object parent) throws IllegalAccessException, InvocationTargetException, NoSuchMethodException
	{
		Field[] declaredFields = object.getClass().getDeclaredFields();

		for (Field declaredField : declaredFields)
		{
			Class<?> declaringType = declaredField.getType();
			String fieldName = declaredField.getName();
			if (!Modifier.isStatic(declaredField.getModifiers()) && !fieldName.equals("verslag"))
			{
				Object value = PropertyUtils.getProperty(object, fieldName);
				if (value != null && (parent == null || !declaringType.isAssignableFrom(parent.getClass())))
				{
					if (value instanceof List)
					{
						for (Object valueElement : (List) value)
						{
							if (!DSValue.class.isAssignableFrom(valueElement.getClass()))
							{
								correctValues(valueElement, object);
							}

						}
					}
					else if (HibernateObject.class.isAssignableFrom(declaringType) && !declaringType.equals(DSValue.class))
					{
						correctValues(value, object);
					}
				}
				else if (parent != null && declaringType.isAssignableFrom(parent.getClass()))
				{

					PropertyUtils.setProperty(object, fieldName, parent);
				}
			}
		}
	}

	@Override
	public boolean magAfronden(VerslagType verslagType, Client client)
	{
		return verwerkVerslagService.getValideScreeningsRonde(verslagType, client, null, null) != null;
	}

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public void verwijderVerslag(Verslag verslag, InstellingGebruiker instellingGebruiker)
	{
		VerslagType verslagType = verslag.getType();
		Class<? extends Verslag<?, ?>> verslagClazz = verslagType.getClazz();
		verslag = hibernateService.load(verslagClazz, verslag.getId());

		heropenRondeEnDossier(verslag, true);

		String melding = createLogMelding(verslag);
		ScreeningRonde screeningRonde = verslag.getScreeningRonde();
		Client client = screeningRonde.getDossier().getClient();
		logService.logGebeurtenis(verslagType.getVerwijderdVerslagLogGebeurtenis(), instellingGebruiker, client, melding, verslagType.getBevolkingsonderzoek());

		verslag.setScreeningRonde(null);
		verslag.setUitvoerderMedewerker(null);
		verslag.setUitvoerderOrganisatie(null);
		verslag.setOntvangenBericht(null);

		hibernateService.saveOrUpdate(screeningRonde);
		hibernateService.delete(verslag);
		hibernateService.delete(verslag.getVerslagContent());

		if (verslag.getType().equals(VerslagType.MAMMA_PA_FOLLOW_UP))
		{
			followUpService.refreshUpdateFollowUpConclusie(client.getMammaDossier());
		}
	}

	private void heropenRondeEnDossier(Verslag verslag, boolean verwijderd)
	{
		VerslagType type = verslag.getType();
		if (type == VerslagType.MDL && VerslagStatus.AFGEROND.equals(verslag.getStatus()))
		{
			MdlVerslag mdlVerslag = (MdlVerslag) HibernateHelper.deproxy(verslag);
			MdlVervolgbeleid vervolgbeleidHuidigVerslag = mdlVerslag.getVervolgbeleid();
			MdlVerslag nieuweLaatsteAfgerondVerslag = null;
			ColonScreeningRonde screeningRonde = mdlVerslag.getScreeningRonde();
			boolean geenAnderVerslagMetDefinitiefVervolgbeleid = true;
			for (ColonVerslag<?> oneOfAllVerslagen : screeningRonde.getVerslagen())
			{
				if (VerslagType.MDL.equals(oneOfAllVerslagen.getType()) && VerslagStatus.AFGEROND.equals(oneOfAllVerslagen.getStatus()) && !oneOfAllVerslagen.equals(mdlVerslag))
				{
					MdlVerslag anderMdlVerslag = (MdlVerslag) HibernateHelper.deproxy(oneOfAllVerslagen);
					if (MdlVervolgbeleid.isDefinitief(anderMdlVerslag.getVervolgbeleid()))
					{
						geenAnderVerslagMetDefinitiefVervolgbeleid = false;
					}
					if (nieuweLaatsteAfgerondVerslag == null || DateUtil.compareAfter(nieuweLaatsteAfgerondVerslag.getDatumOnderzoek(), anderMdlVerslag.getDatumOnderzoek()))
					{
						nieuweLaatsteAfgerondVerslag = anderMdlVerslag;
					}
				}
			}
			ColonDossier dossier = screeningRonde.getDossier();
			if (MdlVervolgbeleid.isDefinitief(vervolgbeleidHuidigVerslag) && geenAnderVerslagMetDefinitiefVervolgbeleid)
			{

				if (ScreeningRondeStatus.AFGEROND.equals(screeningRonde.getStatus()))
				{
					screeningRonde.setStatus(ScreeningRondeStatus.LOPEND);
					screeningRonde.setAfgerondReden(null);
					hibernateService.saveOrUpdate(screeningRonde);

					if (DossierStatus.INACTIEF.equals(dossier.getStatus()))
					{
						dossier.setStatus(DossierStatus.ACTIEF);
						dossier.setInactiveerReden(null);
						dossier.setInactiefVanaf(null);
						dossier.setInactiefTotMet(null);
						hibernateService.saveOrUpdate(dossier);
					}
				}
			}
			if (nieuweLaatsteAfgerondVerslag != null)
			{
				colonDossierService.setVolgendeUitnodigingVoorVerslag(nieuweLaatsteAfgerondVerslag);
			}
			else
			{
				ColonIntakeAfspraak laatsteAfspraak = screeningRonde.getLaatsteAfspraak();
				if (laatsteAfspraak != null)
				{
					colonDossierService.setVolgendeUitnodingVoorConclusie(laatsteAfspraak);
				}
			}
			verwerkVerslagService.ontkoppelOfVerwijderComplicaties(mdlVerslag);
			if (verwijderd)
			{
				screeningRonde.getVerslagen().remove(mdlVerslag);
			}
		}
		else if (type == VerslagType.PA_LAB && verwijderd)
		{
			PaVerslag paVerslag = (PaVerslag) HibernateHelper.deproxy(verslag);
			ColonScreeningRonde screeningRonde = paVerslag.getScreeningRonde();
			screeningRonde.getVerslagen().remove(paVerslag);
		}
		else if (type == VerslagType.MAMMA_PA_FOLLOW_UP)
		{
			MammaFollowUpVerslag followupVerslag = (MammaFollowUpVerslag) HibernateHelper.deproxy(verslag);
			MammaScreeningRonde screeningRonde = followupVerslag.getScreeningRonde();
			MammaScreeningRonde screeningrondeVoorFollowUp = mammaBeoordelingDao.getScreeningrondeVoorFollowUp(screeningRonde.getDossier().getClient(), null);
			if (screeningRonde.equals(screeningrondeVoorFollowUp))
			{
				screeningRonde.setFollowUpConclusieStatus(null);
				screeningRonde.setFollowUpConclusieStatusGewijzigdOp(null);
			}
			if (verwijderd)
			{
				screeningRonde.getFollowUpVerslagen().remove(followupVerslag);
			}
		}
	}

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public <V extends Verslag<?, ?>> V heropenVerslag(V verslag, InstellingGebruiker instellingGebruiker)
	{
		VerslagType verslagType = verslag.getType();
		Class<? extends Verslag<?, ?>> verslagClazz = verslagType.getClazz();
		verslag = (V) hibernateService.load(verslagClazz, verslag.getId());
		heropenRondeEnDossier(verslag, false);

		verslag.setStatus(VerslagStatus.IN_BEWERKING);
		hibernateService.saveOrUpdate(verslag);
		refreshUpdateFollowUpConclusie(verslag);

		Client client = verslag.getScreeningRonde().getDossier().getClient();
		String melding = createLogMelding(verslag);
		logService.logGebeurtenis(verslagType.getHeropendVerslagLogGebeurtenis(), instellingGebruiker, client, melding, verslagType.getBevolkingsonderzoek());
		return verslag;
	}

	private String createLogMelding(Verslag verslag)
	{
		String melding = "";
		OntvangenCdaBericht ontvangenBericht = verslag.getOntvangenBericht();
		if (ontvangenBericht != null)
		{
			melding = "Electronisch bericht: berichtId: " + ontvangenBericht.getBerichtId() + ", setId: " + ontvangenBericht.getSetId() + ", versie: "
				+ ontvangenBericht.getVersie() + ",";
		}
		else
		{
			melding = "Handmatige invoer: ";
		}
		Date datumOnderzoek = verslag.getDatumOnderzoek();
		if (datumOnderzoek != null)
		{
			melding += " datum onderzoek " + Constants.getDateFormat().format(datumOnderzoek);
		}
		if (verslag.getId() == null)
		{
			melding = "Nieuw. " + melding;
		}
		return melding;
	}

	@Override
	public void preFillAntwoorden(Verslag verslag, FormulierResultaatImpl formulierResultaat, Gebruiker gebruiker)
	{
		if (verslag.getUitvoerderMedewerker() != null)
		{
			GebruikerAntwoord antwoord = new GebruikerAntwoord();
			antwoord.setValue(verslag.getUitvoerderMedewerker());
			String uitvoerderMedewerkerKey = null;
			if (verslag.getType().equals(VerslagType.MDL))
			{
				uitvoerderMedewerkerKey = VRAAG_ENDOSCOPIST;
			}
			else
			{
				uitvoerderMedewerkerKey = VRAAG_PATHOLOOG;
			}
			VraagInstantieImpl<Gebruiker> vraagInstatie = formulierService.findVraagInstantieByIdentifier(formulierResultaat.getFormulierInstantie(), uitvoerderMedewerkerKey);
			antwoord.setVraagInstantie(vraagInstatie);

			formulierResultaat.getAntwoorden().add(antwoord);
		}
		else
		{

			Functie functie = gebruiker.getFunctie();
			if (functie != null)
			{
				GebruikerAntwoord antwoord = new GebruikerAntwoord();
				if (functie.getNaam().equals(Functie.ENDOSCOPIST) && verslag.getType().equals(VerslagType.MDL))
				{
					antwoord.setValue(gebruiker);
					VraagInstantieImpl<Gebruiker> vraagInstantie = formulierService.findVraagInstantieByIdentifier(formulierResultaat.getFormulierInstantie(), VRAAG_ENDOSCOPIST);
					antwoord.setVraagInstantie(vraagInstantie);
					formulierResultaat.getAntwoorden().add(antwoord);

				}
				else if (functie.getNaam().equals(Functie.PATHOLOOG) && verslag.getType().equals(VerslagType.PA_LAB))
				{
					antwoord.setValue(gebruiker);
					VraagInstantieImpl<Gebruiker> vraagInstantie = formulierService.findVraagInstantieByIdentifier(formulierResultaat.getFormulierInstantie(), VRAAG_PATHOLOOG);
					antwoord.setVraagInstantie(vraagInstantie);
					formulierResultaat.getAntwoorden().add(antwoord);
				}
			}

		}

		switch (verslag.getType())
		{
		case MDL:
			MdlVerslag mdlVerslag = (MdlVerslag) HibernateHelper.deproxy(verslag);
			MdlVerslagContent mdlVerslagContent = mdlVerslag.getVerslagContent();
		{
			BooleanAntwoord antwoord = new BooleanAntwoord();
			antwoord.setValue(Boolean.FALSE);

			if (mdlVerslagContent.getColoscopieMedischeObservatie() != null && mdlVerslagContent.getColoscopieMedischeObservatie().getMedicatie() != null
				&& mdlVerslagContent.getColoscopieMedischeObservatie().getMedicatie().getMedicatiemiddel() != null)
			{
				antwoord.setValue(mdlVerslagContent.getColoscopieMedischeObservatie().getMedicatie().getMedicatiemiddel().size() > 0);
			}

			VraagInstantieImpl<Boolean> vraagInstatie = formulierService.findVraagInstantieByIdentifier(formulierResultaat.getFormulierInstantie(), VRAAG_SEDATIE_JA_NEE);
			antwoord.setVraagInstantie(vraagInstatie);
			formulierResultaat.getAntwoorden().add(antwoord);
		}
		{
			BooleanAntwoord antwoord = new BooleanAntwoord();
			antwoord.setValue(Boolean.FALSE);

			if (mdlVerslagContent.getLaesiecoloscopiecentrum() != null)
			{
				antwoord.setValue(mdlVerslagContent.getLaesiecoloscopiecentrum().size() > 0);
			}

			VraagInstantieImpl<Boolean> vraagInstatie = formulierService.findVraagInstantieByIdentifier(formulierResultaat.getFormulierInstantie(), Constants.VRAAG_LAESIE_JA_NEE);
			antwoord.setVraagInstantie(vraagInstatie);
			formulierResultaat.getAntwoorden().add(antwoord);
		}
		{
			BooleanAntwoord antwoord = new BooleanAntwoord();
			antwoord.setValue(Boolean.FALSE);

			if (mdlVerslagContent.getVerrichting() != null && mdlVerslagContent.getVerrichting().getIncidentcomplicatie() != null)
			{
				antwoord.setValue(mdlVerslagContent.getVerrichting().getIncidentcomplicatie().size() > 0);
			}

			VraagInstantieImpl<Boolean> vraagInstatie = formulierService.findVraagInstantieByIdentifier(formulierResultaat.getFormulierInstantie(),
				Constants.VRAAG_INCIDENT_COMPLICATIE_JA_NEE);
			antwoord.setVraagInstantie(vraagInstatie);
			formulierResultaat.getAntwoorden().add(antwoord);
		}
			Date datumOnderzoek = verslag.getDatumOnderzoek();
			if (datumOnderzoek != null)
			{
				DateAntwoord antwoord = new DateAntwoord();
				antwoord.setValue(datumOnderzoek);

				VraagInstantieImpl<Date> vraagInstatie = formulierService.findVraagInstantieByIdentifier(formulierResultaat.getFormulierInstantie(),
					Constants.VRAAG_DATUM_VERRICHTING);
				vraagInstatie.setVerplichting(new VerplichtingImpl());
				antwoord.setVraagInstantie(vraagInstatie);
				formulierResultaat.getAntwoorden().add(antwoord);
			}
			String patientnummer = mdlVerslag.getPatientnummer();
			if (StringUtils.isNotBlank(patientnummer))
			{
				StringAntwoord antwoord = new StringAntwoord();
				antwoord.setValue(patientnummer);

				VraagInstantieImpl<String> vraagInstatie = formulierService.findVraagInstantieByIdentifier(formulierResultaat.getFormulierInstantie(), PATIENTNUMMER);
				vraagInstatie.setVerplichting(new VerplichtingImpl());
				antwoord.setVraagInstantie(vraagInstatie);
				formulierResultaat.getAntwoorden().add(antwoord);
			}
			break;
		case PA_LAB:
			PaVerslagContent paVerslagContent = (PaVerslagContent) HibernateHelper.deproxy(verslag.getVerslagContent());
		{
			List<PaPathologieProtocolColonbioptperPoliep> potjes = paVerslagContent.getPathologieProtocolColonbioptperPoliep();
			PalgaNumberAntwoord antwoord = new PalgaNumberAntwoord();
			Integer start = Integer.valueOf(40);
			for (PaPathologieProtocolColonbioptperPoliep potje : potjes)
			{
				String nummerPotjeMateriaal = potje.getNummerPotjeMateriaal();
				if (nummerPotjeMateriaal == null)
				{
					nummerPotjeMateriaal = "1";
				}

				Integer potjeNummer = Integer.valueOf(nummerPotjeMateriaal);
				if (start > potjeNummer)
				{
					start = potjeNummer;
				}
			}
			if (start < 40)
			{
				String stringValue = RomanNumeral.toRoman(start);
				if (potjes.size() > 1)
				{
					stringValue += " - " + RomanNumeral.toRoman(start + potjes.size() - 1);
				}
				antwoord.setValue(new PalgaNumber(potjes.size(), stringValue));

				VraagInstantieImpl<PalgaNumber> vraagInstatie = formulierService.findVraagInstantieByIdentifier(formulierResultaat.getFormulierInstantie(), VRAAG_AANTAL_POTJES);
				antwoord.setVraagInstantie(vraagInstatie);
				formulierResultaat.getAntwoorden().add(antwoord);
			}
		}

			break;
		case CERVIX_CYTOLOGIE:
			break;
		case MAMMA_PA_FOLLOW_UP:
			break;
		}
	}

	@Override
	public List<MdlVerslag> getAlleMdlVerslagenVanClient(Client client)
	{
		return verslagDao.getAlleMdlVerslagenVanClient(client);
	}

	@Override
	public List<OntvangenCdaBericht> searchBerichten(BerichtZoekFilter filter, long first, long count, String property, boolean ascending)
	{
		return verslagDao.searchBerichten(filter, first, count, property, ascending);
	}

	@Override
	public long countBerichten(BerichtZoekFilter filter)
	{
		return verslagDao.countBerichten(filter);
	}

	@Override
	public void herverwerkAlleBerichten(BerichtZoekFilter filter)
	{
		final List<Object> idsEnBerichtTypen = verslagDao.getIdsEnBerichtTypen(filter);

		executorService.submit(new Runnable()
		{
			@Override
			public void run()
			{
				try
				{
					for (Object stiekumEenArray : idsEnBerichtTypen)
					{
						Object[] idEnBerichtType = (Object[]) stiekumEenArray;
						Long id = (Long) idEnBerichtType[0];
						BerichtType berichtType = BerichtType.valueOf((String) idEnBerichtType[1]);
						switch (berichtType)
						{
						case MDL_VERSLAG:
						case PA_LAB_VERSLAG:
							cdaBerichtToBatchService.queueColonCDABericht(id);
							break;
						case CERVIX_CYTOLOGIE_VERSLAG:
							cdaBerichtToBatchService.queueCervixCDABericht(id);
							break;
						case MAMMA_PA_FOLLOW_UP_VERSLAG:
							cdaBerichtToBatchService.queueMammaCDABericht(id);
							break;
						}
						Thread.sleep(1000);
						LOG.info("Bericht met ID " + id + " wordt opnieuw aangeboden");
					}
				}
				catch (Exception e) 
				{
					LOG.error("Er is een onvoorziene crash geweest in de herverwerkAlleBerichten Thread, deze is nu gesloten. " + e.getMessage(), e);
				}
			}
		});

	}

	@Override
	public <V extends Verslag<?, ?>> List<V> zoekVerslagen(V zoekObject, int first, int count, String property, boolean ascending)
	{
		return verslagDao.zoekVerslagen(ModelProxyHelper.deproxy(zoekObject), first, count, property, ascending);
	}

	@Override
	public <V extends Verslag<?, ?>> long countVerslagen(V zoekObject)
	{
		return verslagDao.countVerslagen(ModelProxyHelper.deproxy(zoekObject));
	}

	private void refreshUpdateFollowUpConclusie(Verslag verslag)
	{
		if (verslag.getType().equals(VerslagType.MAMMA_PA_FOLLOW_UP))
		{
			followUpService.refreshUpdateFollowUpConclusie((MammaDossier) verslag.getScreeningRonde().getDossier());
		}
	}
}
