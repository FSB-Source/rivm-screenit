package nl.rivm.screenit.main.service.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
 * %%
 * Copyright (C) 2012 - 2025 Facilitaire Samenwerking Bevolkingsonderzoek
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
import java.util.stream.Collectors;

import nl.rivm.screenit.Constants;
import nl.rivm.screenit.main.service.FormulierService;
import nl.rivm.screenit.main.service.VerslagService;
import nl.rivm.screenit.model.BerichtZoekFilter;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.Functie;
import nl.rivm.screenit.model.Gebruiker;
import nl.rivm.screenit.model.InstellingGebruiker;
import nl.rivm.screenit.model.berichten.Verslag;
import nl.rivm.screenit.model.berichten.cda.OntvangenCdaBericht;
import nl.rivm.screenit.model.berichten.cda.OntvangenCdaBericht_;
import nl.rivm.screenit.model.berichten.enums.BerichtType;
import nl.rivm.screenit.model.berichten.enums.VerslagStatus;
import nl.rivm.screenit.model.berichten.enums.VerslagType;
import nl.rivm.screenit.model.cervix.CervixCytologieVerslag;
import nl.rivm.screenit.model.colon.ColonVerslag;
import nl.rivm.screenit.model.colon.ColonVerslag_;
import nl.rivm.screenit.model.colon.MdlVerslag;
import nl.rivm.screenit.model.colon.PaVerslag;
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
import nl.rivm.screenit.model.verslag.DSValue;
import nl.rivm.screenit.model.verslag.VerslagContent;
import nl.rivm.screenit.repository.algemeen.OntvangenCdaBerichtRepository;
import nl.rivm.screenit.repository.cervix.CervixCytologieVerslagRepository;
import nl.rivm.screenit.repository.colon.ColonMdlVerslagRepository;
import nl.rivm.screenit.repository.colon.ColonPaVerslagRepository;
import nl.rivm.screenit.repository.colon.ColonVerslagRepository;
import nl.rivm.screenit.repository.mamma.MammaFollowUpVerslagRepository;
import nl.rivm.screenit.service.BaseVerslagService;
import nl.rivm.screenit.service.BerichtToBatchService;
import nl.rivm.screenit.service.LogService;
import nl.rivm.screenit.service.VerwerkVerslagService;
import nl.rivm.screenit.service.mamma.MammaBaseFollowUpService;
import nl.rivm.screenit.specification.cervix.CervixVerslagSpecification;
import nl.rivm.screenit.specification.colon.ColonVerslagSpecification;
import nl.rivm.screenit.specification.mamma.MammaFollowUpVerslagSpecification;
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
import nl.topicuszorg.hibernate.object.model.AbstractHibernateObject_;
import nl.topicuszorg.hibernate.object.model.HibernateObject;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;

import org.apache.commons.beanutils.PropertyUtils;
import org.apache.commons.lang.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Sort;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import static java.util.Arrays.stream;
import static nl.rivm.screenit.specification.algemeen.OntvangenCdaBerichtSpecification.maakZoekSpecification;
import static nl.rivm.screenit.specification.colon.ColonVerslagSpecification.heeftClientIdInMdlVerslag;
import static nl.rivm.screenit.specification.colon.ColonVerslagSpecification.heeftClientIdInPaVerslag;
import static nl.rivm.screenit.specification.colon.ColonVerslagSpecification.heeftColonDossier;
import static nl.rivm.screenit.specification.colon.ColonVerslagSpecification.heeftTypeInMdlVerslag;
import static nl.rivm.screenit.specification.colon.ColonVerslagSpecification.heeftTypeInPaVerslag;
import static nl.rivm.screenit.specification.colon.ColonVerslagSpecification.heeftVerslagStatus;
import static org.springframework.data.domain.Sort.Direction.DESC;

@Service
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
	private FormulierService formulierService;

	@Autowired
	private LogService logService;

	@Autowired
	private BerichtToBatchService cdaBerichtToBatchService;

	@Autowired(required = false)
	private MammaBaseFollowUpService followUpService;

	@Autowired
	private BaseVerslagService baseVerslagService;

	@Autowired
	private ColonMdlVerslagRepository mdlVerslagRepository;

	@Autowired
	private OntvangenCdaBerichtRepository ontvangenCdaBerichtRepository;

	@Autowired
	private ColonPaVerslagRepository paVerslagRepository;

	@Autowired
	private CervixCytologieVerslagRepository cervixCytologieVerslagRepository;

	@Autowired
	private MammaFollowUpVerslagRepository mammaVerslagRepository;

	@Autowired
	private ColonVerslagRepository colonVerslagRepository;

	@Override
	@Transactional
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

		var verslag = verslagContent.getVerslag();

		for (Antwoord<?> antwoord : resultaat.getAntwoorden())
		{
			VraagDefinitie<?> vraagDefinitie = antwoord.getVraagInstantie().getVraagDefinitie();
			if (vraagDefinitie instanceof IdentifierElement identifierElement)
			{
				if (identifierElement.getIdentifier() != null)
				{
					switch (identifierElement.getIdentifier())
					{
					case VRAAG_PATHOLOOG:
					case VRAAG_ENDOSCOPIST:
						if (antwoord instanceof GebruikerAntwoord gebruikerAntwoord)
						{
							Gebruiker gebruiker = gebruikerAntwoord.getValue();
							if (gebruiker != null)
							{
								verslag.setUitvoerderMedewerker(gebruiker);
								verslag.setUitvoerderOrganisatie(verslag.getInvoerder().getOrganisatie());
							}
						}
						break;
					case VRAAG_AANTAL_POTJES:
						if (antwoord instanceof PalgaNumberAntwoord palgaNumberAntwoord)
						{
							PalgaNumber palgaNumber = palgaNumberAntwoord.getValue();
							Integer waarde = 0;
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
						if (antwoord instanceof DateAntwoord dateAntwoord)
						{
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
						if (antwoord instanceof BooleanAntwoord booleanAntwoord)
						{
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
										medicatie.setMedicatiemiddel(new ArrayList<>());
									}
									medicatie.setMateVanSedatie(null);
								}
							}
						}
						break;
					case Constants.VRAAG_LAESIE_JA_NEE:
						if (antwoord instanceof BooleanAntwoord booleanAntwoord)
						{
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
									mdlVerslagContent.setLaesiecoloscopiecentrum(new ArrayList<>());
								}
							}
						}
						break;
					case Constants.VRAAG_INCIDENT_COMPLICATIE_JA_NEE:
						if (antwoord instanceof BooleanAntwoord booleanAntwoord)
						{
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
									mdlVerslagContent.getVerrichting().setIncidentcomplicatie(new ArrayList<>());
								}
							}
						}
						break;
					case PATIENTNUMMER:
						if (antwoord instanceof StringAntwoord stringAntwoord)
						{
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
		String createMelding = baseVerslagService.createLogMelding(verslag);
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
		else if (verslag.getType() == VerslagType.MAMMA_PA_FOLLOW_UP_MONITOR)
		{
			throw new IllegalStateException("BK PA Landelijke Monitor verslagen mogen niet handmatig ingevoerd of aangepast worden");
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
	@Transactional
	public <V extends Verslag<?, ?>> V heropenVerslag(V verslag, InstellingGebruiker instellingGebruiker)
	{
		VerslagType verslagType = verslag.getType();
		Class<? extends Verslag<?, ?>> verslagClazz = verslagType.getClazz();
		verslag = (V) hibernateService.load(verslagClazz, verslag.getId());
		baseVerslagService.heropenRondeEnDossier(verslag);

		verslag.setStatus(VerslagStatus.IN_BEWERKING);
		hibernateService.saveOrUpdate(verslag);
		refreshUpdateFollowUpConclusie(verslag);

		Client client = verslag.getScreeningRonde().getDossier().getClient();
		String melding = baseVerslagService.createLogMelding(verslag);
		logService.logGebeurtenis(verslagType.getHeropendVerslagLogGebeurtenis(), instellingGebruiker, client, melding, verslagType.getBevolkingsonderzoek());
		return verslag;
	}

	private void refreshUpdateFollowUpConclusie(Verslag<?, ?> verslag)
	{
		if (verslag.getType() == VerslagType.MAMMA_PA_FOLLOW_UP)
		{
			followUpService.refreshUpdateFollowUpConclusie((MammaDossier) verslag.getScreeningRonde().getDossier());
		}
	}

	@Override
	public void preFillAntwoorden(Verslag verslag, FormulierResultaatImpl formulierResultaat, Gebruiker gebruiker)
	{
		if (verslag.getUitvoerderMedewerker() != null)
		{
			GebruikerAntwoord antwoord = new GebruikerAntwoord();
			antwoord.setValue(verslag.getUitvoerderMedewerker());
			String uitvoerderMedewerkerKey;
			if (verslag.getType().equals(VerslagType.MDL))
			{
				uitvoerderMedewerkerKey = VRAAG_ENDOSCOPIST;
			}
			else
			{
				uitvoerderMedewerkerKey = VRAAG_PATHOLOOG;
			}
			VraagInstantieImpl<Gebruiker> vraagInstantie = formulierService.findVraagInstantieByIdentifier(formulierResultaat.getFormulierInstantie(), uitvoerderMedewerkerKey);
			antwoord.setVraagInstantie(vraagInstantie);

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
				antwoord.setValue(!mdlVerslagContent.getColoscopieMedischeObservatie().getMedicatie().getMedicatiemiddel().isEmpty());
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
				antwoord.setValue(!mdlVerslagContent.getLaesiecoloscopiecentrum().isEmpty());
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
				antwoord.setValue(!mdlVerslagContent.getVerrichting().getIncidentcomplicatie().isEmpty());
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
			var start = 40;
			for (PaPathologieProtocolColonbioptperPoliep potje : potjes)
			{
				String nummerPotjeMateriaal = potje.getNummerPotjeMateriaal();
				if (nummerPotjeMateriaal == null)
				{
					nummerPotjeMateriaal = "1";
				}

				var potjeNummer = Integer.valueOf(nummerPotjeMateriaal);
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
		case MAMMA_PA_FOLLOW_UP:
		case MAMMA_PA_FOLLOW_UP_MONITOR:
			break;
		}
	}

	@Override
	public List<MdlVerslag> getAlleMdlVerslagenVanClient(Client client)
	{
		var spec = heeftColonDossier(client.getColonDossier()).and(heeftVerslagStatus(VerslagStatus.AFGEROND));
		return mdlVerslagRepository.findAll(spec, Sort.by(DESC, ColonVerslag_.DATUM_ONDERZOEK));
	}

	@Override
	public List<OntvangenCdaBericht> zoekBerichten(BerichtZoekFilter filter, long first, long count, String property, boolean ascending)
	{
		var sort = Sort.by(ascending ? Sort.Direction.ASC : Sort.Direction.DESC, property);
		return ontvangenCdaBerichtRepository.findWith(maakZoekSpecification(filter), q -> q.sortBy(sort)).all(first, count);
	}

	@Override
	public long countBerichten(BerichtZoekFilter filter)
	{
		return ontvangenCdaBerichtRepository.count(maakZoekSpecification(filter));
	}

	@Override
	@Transactional
	public void herverwerkAlleBerichten(BerichtZoekFilter filter)
	{
		var idsEnBerichtTypen = ontvangenCdaBerichtRepository.findWith(maakZoekSpecification(filter), Object[].class,
			q -> q.projections((cb, r) -> List.of(r.get(AbstractHibernateObject_.id), r.get(OntvangenCdaBericht_.berichtType)))).all();
		stream(BerichtType.values()).forEach(berichtType -> berichtenOpnieuwVerwerken(
			idsEnBerichtTypen.stream()
				.filter(o -> (((Object[]) o)[1]) == berichtType)
				.map(o -> ((Long) ((Object[]) o)[0]))
				.collect(Collectors.toList()),
			berichtType.getBevolkingsonderzoek()));
	}

	@Override
	public <V extends Verslag<?, ?>> List<V> zoekVerslagen(V zoekCriteria, int first, int aantal, String property, boolean ascending)
	{
		var sort = Sort.by(ascending ? Sort.Direction.ASC : Sort.Direction.DESC, property);

		if (zoekCriteria instanceof MdlVerslag)
		{
			var specification = getZoekMdlVerslagenSpecification((MdlVerslag) zoekCriteria);
			return (List<V>) mdlVerslagRepository.findWith(specification, q -> q.sortBy(sort)).all(first, aantal);
		}

		if (zoekCriteria instanceof PaVerslag)
		{
			var specification = getZoekPaVerslagenSpecification((PaVerslag) zoekCriteria);
			return (List<V>) paVerslagRepository.findWith(specification, q -> q.sortBy(sort)).all(first, aantal);
		}

		if (zoekCriteria instanceof CervixCytologieVerslag)
		{
			var specification = getZoekCervixVerslagenSpecification((CervixCytologieVerslag) zoekCriteria);
			return (List<V>) cervixCytologieVerslagRepository.findWith(specification, q -> q.sortBy(sort)).all(first, aantal);
		}

		if (zoekCriteria instanceof MammaFollowUpVerslag)
		{
			var specification = getZoekFollowUpVerslagenSpecification((MammaFollowUpVerslag) zoekCriteria);
			return (List<V>) mammaVerslagRepository.findWith(specification, q -> q.sortBy(sort)).all(first, aantal);
		}

		if (zoekCriteria instanceof ColonVerslag)
		{
			var specification = getZoekColonVerslagenSpecification((ColonVerslag) zoekCriteria);
			var colonVerslagen = colonVerslagRepository.findWith(specification, q -> q.sortBy(sort)).all(first, aantal);
			return colonVerslagen.stream().map(v -> (V) v).collect(Collectors.toList());
		}

		throw new IllegalArgumentException("Onbekend verslag type: " + zoekCriteria.getClass().getName());
	}

	private Specification<MdlVerslag> getZoekMdlVerslagenSpecification(MdlVerslag zoekObject)
	{
		var specification = heeftClientIdInMdlVerslag(zoekObject.getScreeningRonde().getDossier().getClient().getId());
		if (zoekObject.getType() != null)
		{
			specification = specification.and(heeftTypeInMdlVerslag(zoekObject.getType()));
		}
		return specification;
	}

	private Specification<PaVerslag> getZoekPaVerslagenSpecification(PaVerslag zoekObject)
	{
		var specification = heeftClientIdInPaVerslag(zoekObject.getScreeningRonde().getDossier().getClient().getId());
		if (zoekObject.getType() != null)
		{
			specification = specification.and(heeftTypeInPaVerslag(zoekObject.getType()));
		}
		return specification;
	}

	private Specification<CervixCytologieVerslag> getZoekCervixVerslagenSpecification(CervixCytologieVerslag zoekObject)
	{
		var specification = CervixVerslagSpecification.heeftClientId(zoekObject.getScreeningRonde().getDossier().getClient().getId());
		if (zoekObject.getType() != null)
		{
			specification = specification.and(CervixVerslagSpecification.heeftType(zoekObject.getType()));
		}
		return specification;
	}

	private Specification<MammaFollowUpVerslag> getZoekFollowUpVerslagenSpecification(MammaFollowUpVerslag zoekObject)
	{
		var specification = MammaFollowUpVerslagSpecification.heeftClientId(zoekObject.getScreeningRonde().getDossier().getClient().getId());
		if (zoekObject.getType() != null)
		{
			specification = specification.and(MammaFollowUpVerslagSpecification.heeftType(zoekObject.getType()));
		}
		return specification;
	}

	private Specification<ColonVerslag> getZoekColonVerslagenSpecification(ColonVerslag zoekObject)
	{
		var specification = ColonVerslagSpecification.heeftClientId(zoekObject.getScreeningRonde().getDossier().getClient().getId());
		if (zoekObject.getType() != null)
		{
			specification = specification.and(ColonVerslagSpecification.heeftType(zoekObject.getType()));
		}
		return specification;
	}

	@Override
	public <V extends Verslag<?, ?>> long countVerslagen(V zoekObject)
	{
		if (zoekObject instanceof MdlVerslag)
		{
			var specification = getZoekMdlVerslagenSpecification((MdlVerslag) zoekObject);
			return mdlVerslagRepository.count(specification);
		}

		if (zoekObject instanceof PaVerslag)
		{
			var specification = getZoekPaVerslagenSpecification((PaVerslag) zoekObject);
			return paVerslagRepository.count(specification);
		}

		if (zoekObject instanceof CervixCytologieVerslag)
		{
			var specification = getZoekCervixVerslagenSpecification((CervixCytologieVerslag) zoekObject);
			return cervixCytologieVerslagRepository.count(specification);
		}

		if (zoekObject instanceof MammaFollowUpVerslag)
		{
			var specification = getZoekFollowUpVerslagenSpecification((MammaFollowUpVerslag) zoekObject);
			return mammaVerslagRepository.count(specification);
		}

		if (zoekObject instanceof ColonVerslag)
		{
			var specification = getZoekColonVerslagenSpecification((ColonVerslag) zoekObject);
			return colonVerslagRepository.count(specification);
		}

		throw new IllegalArgumentException("Onbekend verslag type: " + zoekObject.getClass().getName());
	}

	@Override
	public void berichtenOpnieuwVerwerken(List<Long> ids, Bevolkingsonderzoek bvo)
	{
		if (!ids.isEmpty())
		{
			baseVerslagService.setBerichtenOpnieuwVerwerken(ids);
			cdaBerichtToBatchService.queueCDABericht(bvo);
		}
	}

	@Override
	public void berichtOpnieuwVerwerken(OntvangenCdaBericht ontvangenCdaBericht)
	{
		berichtenOpnieuwVerwerken(List.of(ontvangenCdaBericht.getId()), ontvangenCdaBericht.getBerichtType().getBevolkingsonderzoek());
	}
}
