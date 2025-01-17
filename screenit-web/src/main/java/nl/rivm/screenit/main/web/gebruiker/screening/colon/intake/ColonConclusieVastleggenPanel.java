package nl.rivm.screenit.main.web.gebruiker.screening.colon.intake;

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

import java.time.LocalDate;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.List;

import nl.rivm.screenit.main.service.colon.ColonDossierService;
import nl.rivm.screenit.main.service.colon.ColonVervolgonderzoekKeuzesDto;
import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.component.AjaxButtonGroup;
import nl.rivm.screenit.main.web.component.BooleanChoiceRenderer;
import nl.rivm.screenit.main.web.component.ConfirmingIndicatingAjaxLink;
import nl.rivm.screenit.main.web.component.ConfirmingIndicatingAjaxSubmitLink;
import nl.rivm.screenit.main.web.component.modal.BootstrapDialog;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.InstellingGebruiker;
import nl.rivm.screenit.model.berichten.enums.VerslagType;
import nl.rivm.screenit.model.colon.ColonBrief;
import nl.rivm.screenit.model.colon.ColonConclusie;
import nl.rivm.screenit.model.colon.ColonIntakeAfspraak;
import nl.rivm.screenit.model.colon.ColonScreeningRonde;
import nl.rivm.screenit.model.colon.IFOBTTest;
import nl.rivm.screenit.model.colon.enums.ColonAfspraakStatus;
import nl.rivm.screenit.model.colon.enums.ColonConclusieOnHoldReden;
import nl.rivm.screenit.model.colon.enums.ColonConclusieType;
import nl.rivm.screenit.model.colon.enums.ColonGeenOnderzoekReden;
import nl.rivm.screenit.model.colon.enums.ColonUitnodigingsintervalType;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.BriefType;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.service.ClientService;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.colon.ColonBaseAfspraakService;
import nl.rivm.screenit.service.colon.ColonDossierBaseService;
import nl.rivm.screenit.util.ColonScreeningRondeUtil;
import nl.rivm.screenit.util.DateUtil;
import nl.rivm.screenit.util.EnumStringUtil;
import nl.rivm.screenit.util.NaamUtil;
import nl.topicuszorg.wicket.hibernate.cglib.ModelProxyHelper;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.wicket.Component;
import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.form.EnumChoiceRenderer;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.markup.html.form.IChoiceRenderer;
import org.apache.wicket.markup.html.panel.EmptyPanel;
import org.apache.wicket.markup.html.panel.Fragment;
import org.apache.wicket.markup.html.panel.GenericPanel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.model.PropertyModel;
import org.apache.wicket.model.StringResourceModel;
import org.apache.wicket.model.util.ListModel;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.wicketstuff.datetime.markup.html.basic.DateLabel;

public abstract class ColonConclusieVastleggenPanel extends GenericPanel<ColonIntakeAfspraak>
{
	private static final List<ColonConclusieType> OPTIES_INTAKE_CONCLUSIE_JA = Arrays.asList(ColonConclusieType.COLOSCOPIE, ColonConclusieType.CT_COLOGRAFIE,
		ColonConclusieType.GEEN_VERVOLGONDERZOEK);

	private static final IModel<List<Boolean>> BOOLEAN_OPTIONS_MODEL = new ListModel<>(Arrays.asList(Boolean.TRUE, Boolean.FALSE));

	private final List<ColonConclusieType> optiesIntakeConclusieNee = new ArrayList<>(
		Arrays.asList(ColonConclusieType.DOORVERWIJZEN_NAAR_ANDER_CENTRUM, ColonConclusieType.NO_SHOW, ColonConclusieType.ON_HOLD));

	private final IModel<List<Boolean>> intakeconclusieOptionsModel = new ListModel<>(new ArrayList<>(Arrays.asList(Boolean.TRUE, Boolean.FALSE)));

	private final boolean verwijzingMoetBevestigdWorden;

	@SpringBean
	private ColonDossierService dossierService;

	@SpringBean
	private ColonDossierBaseService dossierBaseService;

	@SpringBean
	private ClientService clientService;

	@SpringBean
	private ICurrentDateSupplier dateSupplier;

	@SpringBean
	private ColonBaseAfspraakService afspraakService;

	private ColonConclusieType origConclusie;

	private Component conclusieContainer;

	private final boolean mdlVerslagVerwerkt;

	private final ColonVervolgonderzoekKeuzesDto vervolgonderzoekDto;

	private ConfirmingIndicatingAjaxSubmitLink<ColonIntakeAfspraak> opslaan;

	private final boolean readOnly;

	private class IntegerChoiceRenderer implements IChoiceRenderer<Integer>
	{
		@Override
		public Object getDisplayValue(Integer object)
		{
			return object != null ? object.toString() : "n.v.t.";
		}

		@Override
		public String getIdValue(Integer object, int index)
		{
			return null;
		}

		@Override
		public Integer getObject(String id, IModel<? extends List<? extends Integer>> choices)
		{
			return null;
		}
	}

	protected ColonConclusieVastleggenPanel(String id, IModel<ColonIntakeAfspraak> model)
	{
		super(id, model);

		ColonIntakeAfspraak afspraak = ModelUtil.nullSafeGet(model);
		ColonConclusie conclusie = afspraak.getConclusie();
		mdlVerslagVerwerkt = ColonScreeningRondeUtil.heeftAfgerondeVerslag(afspraak.getColonScreeningRonde(), VerslagType.MDL);
		vervolgonderzoekDto = new ColonVervolgonderzoekKeuzesDto();

		if (conclusie == null)
		{
			conclusie = new ColonConclusie();
			afspraak.setConclusie(conclusie);
			conclusie = afspraak.getConclusie(); 
			readOnly = false;
			verwijzingMoetBevestigdWorden = false;
		}
		else
		{
			origConclusie = conclusie.getType();

			verwijzingMoetBevestigdWorden =
				ColonConclusieType.DOORVERWIJZEN_NAAR_ANDER_CENTRUM == origConclusie && Boolean.FALSE.equals(afspraak.getConclusie().getDoorverwijzingBevestigd());
			readOnly = !verwijzingMoetBevestigdWorden && !dossierService.magConclusieAanpassenVerwijderen(afspraak, origConclusie);
			if (verwijzingMoetBevestigdWorden)
			{
				intakeconclusieOptionsModel.getObject().remove(Boolean.TRUE);
				optiesIntakeConclusieNee.remove(ColonConclusieType.ON_HOLD);
				vervolgonderzoekDto.isDoorverwezenDoorInfolijn = true;
			}
		}
		if (conclusie.getDatum() == null)
		{
			conclusie.setDatum(dateSupplier.getDate());
		}
		if (conclusie.getInstellingGebruiker() == null)
		{
			conclusie.setInstellingGebruiker(ScreenitSession.get().getLoggedInInstellingGebruiker());
		}
		Form<ColonIntakeAfspraak> form = new ConclusieForm("form", model);
		add(form);
		Client client = model.getObject().getColonScreeningRonde().getDossier().getClient();
		add(new Label("client.persoon.bsn"));
		add(new Label("client.persoon.geboortedatum", DateUtil.getGeboortedatum(client)));
	}

	private class ConclusieForm extends Form<ColonIntakeAfspraak>
	{

		private static final long serialVersionUID = 1L;

		private final BootstrapDialog dialog;

		public ConclusieForm(String id, IModel<ColonIntakeAfspraak> model)
		{
			super(id, model);

			dialog = new BootstrapDialog("dialog");
			add(dialog);

			ColonIntakeAfspraak intakeAfspraak = ModelUtil.nullSafeGet(model);

			initKeuzes();

			add(new VraagFragment("eersteVraag", "intakeConclusie", intakeconclusieOptionsModel)
			{
				@Override
				protected Component onJa(String id)
				{
					return new ConclusieFragment(id, OPTIES_INTAKE_CONCLUSIE_JA);
				}

				@Override
				protected Component onNee(String id)
				{
					return new ConclusieFragment(id, optiesIntakeConclusieNee);
				}

			});
			add(DateLabel.forDatePattern("printDatum", Model.of(getOngunstigeUitslagBriefDatum(intakeAfspraak)), "dd-MM-yyyy"));

			add(DateLabel.forDatePattern("conclusie.datum", "dd-MM-yyyy HH:mm"));
			add(DateLabel.forDatePattern("datumEerstOngunstigeUitslag", Model.of(getDatumEersteOngunstigeUitslagInRonde(intakeAfspraak)), "dd-MM-yyyy"));

			Client client = intakeAfspraak.getColonScreeningRonde().getDossier().getClient();
			add(new Label("client.persoon.achternaam", NaamUtil.titelVoorlettersTussenvoegselEnAanspreekAchternaam(client)));
			add(new Label("conclusie.instellingGebruiker.medewerker.naamVolledig"));
			addOudeAfspraak();

			List<Integer> choices = new ArrayList<>();
			choices.add(null);
			for (int i = 1; i <= 5; i++)
			{
				choices.add(i);
			}
			AjaxButtonGroup<Integer> asaScoreChoice = new AjaxButtonGroup<>("conclusie.asaScore", new ListModel<>(choices), new IntegerChoiceRenderer());
			asaScoreChoice.setEnabled(!mdlVerslagVerwerkt);
			add(asaScoreChoice);

			add(new AjaxButtonGroup<>("bezwaar", BOOLEAN_OPTIONS_MODEL, new BooleanChoiceRenderer()).setEnabled(!mdlVerslagVerwerkt)
				.setVisible(intakeAfspraak.isBezwaar()));

			opslaan = new ConfirmingIndicatingAjaxSubmitLink<>("conclusieOpslaan", dialog, null)
			{
				@Override
				public void onYesClick(AjaxRequestTarget target)
				{
					if (!ColonConclusieVastleggenPanel.this.hasErrorMessage())
					{
						ColonIntakeAfspraak afspraak = ConclusieForm.this.getModelObject();
						ColonConclusie conclusie = afspraak.getConclusie();

						InstellingGebruiker loggedInInstellingGebruiker = ScreenitSession.get().getLoggedInInstellingGebruiker();
						conclusie.setInstellingGebruiker(loggedInInstellingGebruiker);
						dossierService.conclusieOpslaan(ModelProxyHelper.deproxy(afspraak), vervolgonderzoekDto, loggedInInstellingGebruiker,
							origConclusie);

						laatMeldingenZien(afspraak);

						close(target);
					}
				}

				private void laatMeldingenZien(ColonIntakeAfspraak afspraak)
				{
					if ((!ColonConclusieType.DOORVERWIJZEN_NAAR_ANDER_CENTRUM.equals(origConclusie))
						&& afspraak.getConclusie() != null
						&& ColonConclusieType.DOORVERWIJZEN_NAAR_ANDER_CENTRUM.equals(afspraak.getConclusie().getType()))
					{
						var regio = afspraak.getKamer().getIntakelocatie().getRegio();
						if (regio != null)
						{
							warn(String.format(getString("bel.infolijn.voor.afspraak"), regio.getTelefoon()));
						}
						else
						{
							warn(getString("bel.infolijn.voor.afspraak.zonder.regio.info"));
						}
					}
				}

				@Override
				protected IModel<String> getHeaderStringModel()
				{
					ColonIntakeAfspraak afspraak = ConclusieForm.this.getModelObject();

					String resourceKey = "";
					ColonUitnodigingsintervalType interval = null;
					if (Boolean.FALSE.equals(vervolgonderzoekDto.redenGeenVervolgOnderzoek))
					{
						interval = ColonUitnodigingsintervalType.INTAKE_GEEN_VERVOLGBELEID_VERZOEK_CLIENT;
						resourceKey = EnumStringUtil.getPropertyString(ColonGeenOnderzoekReden.VERZOEK_CLIENT) + ".gevolg";
					}
					else if (Boolean.TRUE.equals(vervolgonderzoekDto.redenGeenVervolgOnderzoek) && Boolean.FALSE.equals(vervolgonderzoekDto.terugNaarScreening))
					{
						interval = ColonUitnodigingsintervalType.INTAKE_GEEN_VERVOLGBELEID_MEDISCHE_REDENEN;
						resourceKey = EnumStringUtil.getPropertyString(ColonGeenOnderzoekReden.MEDISCHE_REDENEN) + ".gevolg";
					}
					else
					{
						interval = ColonGeenOnderzoekReden.getTerugNaarScreeningReden(vervolgonderzoekDto.aantalJarenTerugNaarScreening).getUitnodigingsintervalType();
						resourceKey = "ColonGeenOnderzoekReden.TERUG_NAAR_SCREENING_x_JAAR.gevolg";
					}
					StringResourceModel resourceModel = new StringResourceModel(resourceKey, ColonConclusieVastleggenPanel.this);
					LocalDate theoretischeDatumVolgendeUitnodiging = dossierBaseService.getTheoretischeDatumVolgendeUitnodiging(afspraak.getColonScreeningRonde().getDossier(),
						interval);
					if (theoretischeDatumVolgendeUitnodiging != null)
					{
						int jaar = theoretischeDatumVolgendeUitnodiging.getYear();
						resourceModel.setParameters("" + jaar);
					}
					return resourceModel;
				}

				@Override
				protected boolean skipConfirmation()
				{
					return vervolgonderzoekDto.redenGeenVervolgOnderzoek == null;
				}
			};
			add(opslaan);
			opslaan.setVisible(!ColonAfspraakStatus.isGeannuleerd(intakeAfspraak.getStatus()) && !mdlVerslagVerwerkt);
			opslaan.setEnabled(intakeAfspraak.getConclusie().getId() != null);
			opslaan.setOutputMarkupId(true);

			if (verwijzingMoetBevestigdWorden)
			{
				opslaan.add(new Label("opslaanTekst", "Bevestigen"));
			}
			else
			{
				opslaan.add(new Label("opslaanTekst", "Opslaan"));
			}
			final ConfirmingIndicatingAjaxLink<ColonIntakeAfspraak> verwijderen = new ConfirmingIndicatingAjaxLink<>("conclusieVerwijderen", dialog,
				"conclusie.verwijderen")
			{
				@Override
				public void onClick(AjaxRequestTarget target)
				{
					ColonIntakeAfspraak afspraak = ConclusieForm.this.getModelObject();

					dossierService.conclusieVerwijderen(afspraak, ScreenitSession.get().getLoggedInInstellingGebruiker(), origConclusie);

					close(target);
				}

				@Override
				protected IModel<String> getContentStringModel()
				{
					return new StringResourceModel("conclusie.verwijderen.content", ColonConclusieVastleggenPanel.this, null);
				}

				@Override
				protected boolean skipConfirmation()
				{
					return origConclusie.equals(ColonConclusieType.ON_HOLD);
				}

			};
			verwijderen.setVisible(dossierService.magConclusieAanpassenVerwijderen(intakeAfspraak, origConclusie)
				&& ScreenitSession.get().checkPermission(Recht.GEBRUIKER_SCREENING_INTAKE_WERKLIJST, Actie.VERWIJDEREN));
			verwijderen.setEnabled(true);
			add(verwijderen);

			add(new WebMarkupContainer("tekstOntvangenMDLverslag").setVisible(mdlVerslagVerwerkt));
			add(new WebMarkupContainer("tekstGeenLaatsteAfspraak")
				.setVisible(!mdlVerslagVerwerkt && !intakeAfspraak.getColonScreeningRonde().getLaatsteAfspraak().equals(intakeAfspraak) && !(getModelObject().getConclusie() != null
					&& ColonConclusieType.DOORVERWIJZEN_NAAR_ANDER_CENTRUM.equals(getModelObject().getConclusie().getType()))));
		}

		private void addOudeAfspraak()
		{
			var oudeAfspraak = afspraakService.zoekBevestigdeDoorverwijzendeAfspraak(getModelObject());

			add(new Label("verwezenDoor", new PropertyModel<>(ModelUtil.sModel(oudeAfspraak), "kamer.intakelocatie.naam")).setVisible(
				oudeAfspraak != null));
		}

		private Date getDatumEersteOngunstigeUitslagInRonde(ColonIntakeAfspraak intakeAfspraak)
		{
			IFOBTTest eersteGunstigeTest = ColonScreeningRondeUtil.getEersteOngunstigeTest(intakeAfspraak.getColonScreeningRonde());
			return eersteGunstigeTest != null ? eersteGunstigeTest.getVerwerkingsDatum() : null;
		}

		private void initKeuzes()
		{
			ColonIntakeAfspraak afspraak = getModelObject();
			ColonConclusie conclusie = afspraak.getConclusie();
			if (conclusie != null && conclusie.getType() != null)
			{
				ColonConclusieType type = conclusie.getType();
				vervolgonderzoekDto.conclusie = type;
				if (optiesIntakeConclusieNee.contains(type))
				{
					vervolgonderzoekDto.intakeConclusie = false;
					vervolgonderzoekDto.verwijzing = type == ColonConclusieType.DOORVERWIJZEN_NAAR_ANDER_CENTRUM ? true : null;
				}
				else if (type == ColonConclusieType.CLIENT_WIL_ANDERE_INTAKELOKATIE)
				{
					vervolgonderzoekDto.intakeConclusie = false;
					vervolgonderzoekDto.verwijzing = false;
					vervolgonderzoekDto.conclusie = ColonConclusieType.DOORVERWIJZEN_NAAR_ANDER_CENTRUM;
				}
				else if (OPTIES_INTAKE_CONCLUSIE_JA.contains(type))
				{
					vervolgonderzoekDto.intakeConclusie = true;
				}
				if (type == ColonConclusieType.GEEN_VERVOLGONDERZOEK)
				{
					vervolgonderzoekDto.redenGeenVervolgOnderzoek = true;
					vervolgonderzoekDto.terugNaarScreening = false;
					switch (conclusie.getGeenOnderzoekReden())
					{
					case VERZOEK_CLIENT:
						vervolgonderzoekDto.redenGeenVervolgOnderzoek = false;
						vervolgonderzoekDto.terugNaarScreening = null;
						break;
					case MEDISCHE_REDENEN:
						break;
					default:
						vervolgonderzoekDto.aantalJarenTerugNaarScreening = Integer.valueOf(conclusie.getGeenOnderzoekReden().name().split("_")[3]);
						vervolgonderzoekDto.terugNaarScreening = true;
						break;
					}
				}
			}
		}

		private Date getOngunstigeUitslagBriefDatum(ColonIntakeAfspraak afspraak)
		{
			Date briefAfgedrukt = null;
			ColonScreeningRonde ronde = afspraak.getColonScreeningRonde();
			if (ronde != null)
			{
				for (ColonBrief brief : ronde.getBrieven())
				{
					if (brief.getIntakeAfspraak() != null && brief.getMergedBrieven() != null && afspraak.getId().equals(brief.getIntakeAfspraak().getId())
						&& BriefType.COLON_UITNODIGING_INTAKE.equals(brief.getBriefType()) && brief.getMergedBrieven().getPrintDatum() != null)
					{
						briefAfgedrukt = brief.getMergedBrieven().getPrintDatum();
					}
				}
			}
			return briefAfgedrukt;
		}
	}

	private class ConclusieFragment extends Fragment
	{

		private static final long serialVersionUID = 1L;

		public ConclusieFragment(String id, List<ColonConclusieType> conclusieMogelijkheden)
		{
			super(id, "conclusieFragment", ColonConclusieVastleggenPanel.this);

			Component conclusieType = new AjaxButtonGroup<>("conclusie.type", new PropertyModel<>(vervolgonderzoekDto, "conclusie"), new ListModel<>(conclusieMogelijkheden),
				new EnumChoiceRenderer<>(this))
			{
				@Override
				protected void onSelectionChanged(ColonConclusieType antwoord, AjaxRequestTarget target, String markupId)
				{
					addOrUpdateContainer(target, antwoord);
					resetNietGeselecteerdeGegevens();
				}
			};
			conclusieType.setEnabled(!readOnly);
			add(conclusieType);

			addOrUpdateContainer(null, vervolgonderzoekDto.conclusie);
		}

		private void addOrUpdateContainer(AjaxRequestTarget target, ColonConclusieType type)
		{
			String containerId = "container";
			Component newContainer = new WebMarkupContainer(containerId);
			boolean opslaanEnabled = true;
			if (type != null)
			{
				switch (type)
				{
				case COLOSCOPIE:
					newContainer = new ConclusieColoscopiePanel(containerId, mdlVerslagVerwerkt);
					break;
				case DOORVERWIJZEN_NAAR_ANDER_CENTRUM:
					opslaanEnabled = false;
				case CLIENT_WIL_ANDERE_INTAKELOKATIE:
					newContainer = new VraagFragment(containerId, "verwijzing", new BooleanChoiceRenderer()
					{
						@Override
						public Object getDisplayValue(Boolean object)
						{
							return Boolean.TRUE.equals(object) ? getString("medischeRedenen") : getString("opVerzoekClient");
						}
					}, BOOLEAN_OPTIONS_MODEL)
					{
						@Override
						protected Component onJa(String id) 
						{
							return doorVerwijzenNaarAnderIntakeLocatie(id);
						}

						@Override
						protected Component onNee(String id) 
						{
							return new GevolgFragment(id,
								new StringResourceModel(EnumStringUtil.getPropertyString(ColonConclusieType.CLIENT_WIL_ANDERE_INTAKELOKATIE) + ".gevolg"));
						}
					};
					break;
				case GEEN_VERVOLGONDERZOEK:
					opslaanEnabled = false;
					newContainer = new VraagFragment(containerId, "redenGeenVervolgOnderzoek", new BooleanChoiceRenderer()
					{
						@Override
						public Object getDisplayValue(Boolean object)
						{
							return Boolean.TRUE.equals(object) ? getString("medischeRedenen") : getString("opVerzoekClient");
						}
					}, BOOLEAN_OPTIONS_MODEL)
					{
						@Override
						protected Component onJa(String id)
						{
							return new VraagFragment(id, "terugNaarScreening", BOOLEAN_OPTIONS_MODEL)
							{

								@Override
								protected Component onJa(String id)
								{
									return new TerugNaarScreeningFragment(id);
								}
							};
						}
					};
					break;
				case ON_HOLD:
					newContainer = new OnHoldRedenFragment(containerId);
					break;
				default:
					break;
				}
			}
			newContainer.setOutputMarkupId(true);
			if (target != null)
			{
				conclusieContainer.replaceWith(newContainer);
				conclusieContainer = newContainer;
				target.add(conclusieContainer);
				opslaan.setEnabled(opslaanEnabled);
				target.add(opslaan);
			}
			else
			{
				conclusieContainer = newContainer;
				add(conclusieContainer);
			}
		}

		private Fragment doorVerwijzenNaarAnderIntakeLocatie(String id)
		{
			var afspraak = ColonConclusieVastleggenPanel.this.getModelObject();
			if (ColonConclusieType.DOORVERWIJZEN_NAAR_ANDER_CENTRUM.equals(origConclusie))
			{
				Boolean doorverwijzingBevestigd = afspraak.getConclusie().getDoorverwijzingBevestigd();
				if (Boolean.TRUE.equals(doorverwijzingBevestigd))
				{
					if (afspraak.getNieuweAfspraak() != null)
					{
						return new VerwijzingFragment(id, readOnly, ColonConclusieVastleggenPanel.this.getModel());
					}
					else
					{
						return new BelInfolijnVoorAfspraak(id);
					}
				}
				else if (Boolean.FALSE.equals(doorverwijzingBevestigd))
				{
					return new VerwijzingFragment(id, readOnly, ColonConclusieVastleggenPanel.this.getModel());
				}
				else
				{
					throw new IllegalStateException("doorverwijzingBevestigd mag niet leeg zijn");
				}
			}
			else
			{
				return new GevolgFragment(id,
					new StringResourceModel(EnumStringUtil.getPropertyString(ColonConclusieType.DOORVERWIJZEN_NAAR_ANDER_CENTRUM) + ".gevolg"));
			}
		}
	}

	private class VraagFragment extends Fragment
	{

		private static final long serialVersionUID = 1L;

		private final IModel<List<Boolean>> opties;

		private final String vraag;

		private final IChoiceRenderer<Boolean> renderer;

		public VraagFragment(String id, String vraag, IModel<List<Boolean>> opties)
		{
			this(id, vraag, new BooleanChoiceRenderer(), opties);
		}

		public VraagFragment(String id, String vraag, IChoiceRenderer<Boolean> renderer, IModel<List<Boolean>> opties)
		{
			super(id, "vraagFragment", ColonConclusieVastleggenPanel.this);
			this.vraag = vraag;
			this.renderer = renderer;
			this.opties = opties;
		}

		@Override
		protected void onInitialize()
		{
			super.onInitialize();
			add(new Label("vraag", getString(vraag)));
			IModel<Boolean> antwoordModel = new PropertyModel<>(vervolgonderzoekDto, vraag);
			setOutputMarkupId(true);
			add(new AjaxButtonGroup<>("antwoord", antwoordModel, opties, renderer)
			{
				@Override
				protected void onSelectionChanged(Boolean antwoord, AjaxRequestTarget target, String markupId)
				{
					resetNietGeselecteerdeGegevens();
					boolean eindIsBereikt = maakVervolgPanel(antwoord);

					target.add(VraagFragment.this);
					opslaan.setEnabled(eindIsBereikt);
					target.add(opslaan);
				}

			}.setEnabled(!readOnly));
			maakVervolgPanel(antwoordModel.getObject());
		}

		private boolean maakVervolgPanel(Boolean antwoord)
		{
			Component newVervolgPannel = null;
			String id = "vervolg";
			if (Boolean.TRUE.equals(antwoord))
			{
				newVervolgPannel = onJa(id);
			}
			else if (Boolean.FALSE.equals(antwoord))
			{
				newVervolgPannel = onNee(id);
			}
			else
			{
				newVervolgPannel = new EmptyPanel(id);
			}
			addOrReplace(newVervolgPannel);
			return antwoord != null
				&& (newVervolgPannel instanceof EmptyPanel || newVervolgPannel instanceof GevolgFragment || newVervolgPannel instanceof VerwijzingFragment);
		}

		protected Component onJa(String id)
		{
			return new EmptyPanel(id);
		}

		protected Component onNee(String id)
		{
			return new EmptyPanel(id);
		}
	}

	private class TerugNaarScreeningFragment extends Fragment
	{
		private static final long serialVersionUID = 1L;

		public TerugNaarScreeningFragment(String id)
		{
			super(id, "terugNaarScreeningFragment", ColonConclusieVastleggenPanel.this);

			IModel<Integer> antwoordModel = new PropertyModel<>(vervolgonderzoekDto, "aantalJarenTerugNaarScreening");
			setOutputMarkupId(true);
			List<Integer> choices = new ArrayList<>();
			for (int i = 2; i <= 10; i++)
			{
				choices.add(i);
			}
			add(new AjaxButtonGroup<>("jaar", antwoordModel, new ListModel<>(choices), new IntegerChoiceRenderer())
			{
				@Override
				protected void onSelectionChanged(Integer antwoord, AjaxRequestTarget target, String markupId)
				{
					opslaan.setEnabled(true);
					target.add(opslaan);
				}

			}.setEnabled(!readOnly));
		}
	}

	private class OnHoldRedenFragment extends Fragment
	{
		public OnHoldRedenFragment(String id)
		{
			super(id, "onHoldRedenFragment", ColonConclusieVastleggenPanel.this);
			setOutputMarkupId(true);
			IModel<ColonConclusieOnHoldReden> conclusieRedenModel = new PropertyModel<>(getModel(), "conclusie.onHoldReden");
			Component onHoldReden = new AjaxButtonGroup<>("conclusie.onHoldReden", conclusieRedenModel, new ListModel<>(Arrays.asList(ColonConclusieOnHoldReden.values())),
				new EnumChoiceRenderer<>(this));
			add(onHoldReden);
		}
	}

	private class BelInfolijnVoorAfspraak extends Fragment
	{
		public BelInfolijnVoorAfspraak(String id)
		{
			super(id, "belInfolijnVoorAfspraak", ColonConclusieVastleggenPanel.this);

			add(new Label("kamer.intakelocatie.regio.telefoon"));
		}
	}

	private class GevolgFragment extends Fragment
	{
		private static final long serialVersionUID = 1L;

		public GevolgFragment(String id, IModel<String> gevolgModel)
		{
			super(id, "gevolgFragment", ColonConclusieVastleggenPanel.this);

			add(new Label("gevolg", gevolgModel));
		}
	}

	private class VerwijzingFragment extends Fragment
	{
		public VerwijzingFragment(String id, boolean readOnly, IModel<ColonIntakeAfspraak> model)
		{
			super(id, "verwijzingFragment", ColonConclusieVastleggenPanel.this, model);
			setVisible(!readOnly);
		}

		@Override
		protected void onInitialize()
		{
			super.onInitialize();

			ColonIntakeAfspraak afspraak = getModelObject();
			if (Boolean.TRUE.equals(afspraak.getConclusie().getDoorverwijzingBevestigd()))
			{
				add(new Label("begeleidendTekst", getString("verwijzingCompleet")));
			}
			else
			{
				add(new Label("begeleidendTekst", getString("bevestigingNodigVoorVerwijzing")).setEscapeModelStrings(false));
			}
			add(new Label("nieuweAfspraak.kamer.intakelocatie.naam"));
			add(new Label("nieuweAfspraak.kamer.naam"));
			add(new Label("nieuweAfspraak.vanaf", DateTimeFormatter.ofPattern("EEEE dd-MM-yyyy HH:mm").format(afspraak.getVanaf())));
		}
	}

	private void resetNietGeselecteerdeGegevens()
	{
		ColonConclusie conclusie = getModelObject().getConclusie();
		if (Boolean.TRUE.equals(vervolgonderzoekDto.intakeConclusie))
		{
			vervolgonderzoekDto.verwijzing = null;
			if (optiesIntakeConclusieNee.contains(vervolgonderzoekDto.conclusie))
			{
				vervolgonderzoekDto.conclusie = null;
			}
		}
		else if (Boolean.FALSE.equals(vervolgonderzoekDto.intakeConclusie))
		{
			if (OPTIES_INTAKE_CONCLUSIE_JA.contains(vervolgonderzoekDto.conclusie))
			{
				vervolgonderzoekDto.conclusie = null;
			}
			if (!ColonConclusieType.DOORVERWIJZEN_NAAR_ANDER_CENTRUM.equals(vervolgonderzoekDto.conclusie))
			{
				vervolgonderzoekDto.verwijzing = null;
			}
		}
		if (vervolgonderzoekDto.conclusie == null || ColonConclusieType.CT_COLOGRAFIE == vervolgonderzoekDto.conclusie)
		{
			vervolgonderzoekDto.redenGeenVervolgOnderzoek = null;
			conclusie.setColoscopieDatumOpVerzoekClient(null);
			conclusie.setDatumColoscopie(null);
		}
		else if (vervolgonderzoekDto.conclusie == ColonConclusieType.GEEN_VERVOLGONDERZOEK)
		{
			conclusie.setColoscopieDatumOpVerzoekClient(null);
			conclusie.setDatumColoscopie(null);
		}
		else if (ColonConclusieType.COLOSCOPIE == vervolgonderzoekDto.conclusie)
		{
			vervolgonderzoekDto.redenGeenVervolgOnderzoek = null;
		}

		if (vervolgonderzoekDto.redenGeenVervolgOnderzoek == null || !vervolgonderzoekDto.redenGeenVervolgOnderzoek)
		{
			vervolgonderzoekDto.terugNaarScreening = null;
		}
		if (vervolgonderzoekDto.terugNaarScreening == null || !vervolgonderzoekDto.terugNaarScreening)
		{
			vervolgonderzoekDto.aantalJarenTerugNaarScreening = null;
		}
		if (!ColonConclusieType.ON_HOLD.equals(conclusie.getType()) && conclusie.getOnHoldReden() != null)
		{
			conclusie.setOnHoldReden(null);
		}
	}

	protected abstract void close(AjaxRequestTarget target);
}
