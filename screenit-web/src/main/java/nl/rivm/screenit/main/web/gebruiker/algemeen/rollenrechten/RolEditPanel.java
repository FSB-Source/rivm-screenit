package nl.rivm.screenit.main.web.gebruiker.algemeen.rollenrechten;

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

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import nl.rivm.screenit.dto.RolDto;
import nl.rivm.screenit.main.service.MedewerkerService;
import nl.rivm.screenit.main.service.RolService;
import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.component.ComponentHelper;
import nl.rivm.screenit.main.web.component.ConfirmingIndicatingAjaxLink;
import nl.rivm.screenit.main.web.component.NaamChoiceRenderer;
import nl.rivm.screenit.main.web.component.dropdown.ScreenitDropdown;
import nl.rivm.screenit.main.web.component.dropdown.ScreenitListMultipleChoice;
import nl.rivm.screenit.main.web.component.modal.BootstrapDialog;
import nl.rivm.screenit.main.web.component.modal.DefaultConfirmCallback;
import nl.rivm.screenit.main.web.component.modal.IDialog;
import nl.rivm.screenit.main.web.gebruiker.gedeeld.formulieren.FormulierIndicatingAjaxSubmitLink;
import nl.rivm.screenit.main.web.security.SecurityConstraint;
import nl.rivm.screenit.mappers.RolMapper;
import nl.rivm.screenit.model.InstellingGebruikerRol;
import nl.rivm.screenit.model.Permissie;
import nl.rivm.screenit.model.Rol;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.model.enums.ToegangLevel;
import nl.rivm.screenit.model.helper.INaamComparator;
import nl.rivm.screenit.model.helper.PermissieComparator;
import nl.rivm.screenit.security.IScreenitRealm;
import nl.rivm.screenit.service.AutorisatieService;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.wicket.hibernate.markup.form.validation.UniqueFieldValidator;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.shiro.authz.AuthorizationInfo;
import org.apache.shiro.cache.Cache;
import org.apache.wicket.Component;
import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.form.AjaxFormComponentUpdatingBehavior;
import org.apache.wicket.ajax.form.OnChangeAjaxBehavior;
import org.apache.wicket.ajax.markup.html.AjaxLink;
import org.apache.wicket.behavior.AttributeAppender;
import org.apache.wicket.extensions.ajax.markup.html.IndicatingAjaxLink;
import org.apache.wicket.feedback.FeedbackMessage;
import org.apache.wicket.feedback.IFeedbackMessageFilter;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.form.ChoiceRenderer;
import org.apache.wicket.markup.html.form.EnumChoiceRenderer;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.markup.html.form.FormComponent;
import org.apache.wicket.markup.html.form.ValidationErrorFeedback;
import org.apache.wicket.markup.html.list.ListItem;
import org.apache.wicket.markup.html.list.ListView;
import org.apache.wicket.markup.html.panel.EmptyPanel;
import org.apache.wicket.markup.html.panel.GenericPanel;
import org.apache.wicket.model.CompoundPropertyModel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.model.PropertyModel;
import org.apache.wicket.model.util.ListModel;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.apache.wicket.util.visit.IVisitor;
import org.apache.wicket.validation.ValidationError;
import org.wicketstuff.shiro.ShiroConstraint;

@SecurityConstraint(actie = Actie.INZIEN, checkScope = true, constraint = ShiroConstraint.HasPermission, recht = Recht.GEBRUIKER_ROLLEN_BEHEREN, bevolkingsonderzoekScopes = {
	Bevolkingsonderzoek.COLON, Bevolkingsonderzoek.CERVIX, Bevolkingsonderzoek.MAMMA })
public class RolEditPanel extends GenericPanel<Rol>
{

	@SpringBean
	private RolService rolService;

	@SpringBean
	private HibernateService hibernateService;

	@SpringBean
	private AutorisatieService autorisatieService;

	@SpringBean
	private MedewerkerService medewerkerService;

	@SpringBean
	private IScreenitRealm realm;

	@SpringBean
	private RolMapper rolMapper;

	@SpringBean(name = "testModus")
	private Boolean testModus;

	private WebMarkupContainer permissiesContainer = null;

	private BootstrapDialog dialog;

	private WebMarkupContainer rechtDropdownContainer;

	private ScreenitDropdown<Recht> rechtDropdown;

	private RolForm rolForm;

	private ScreenitListMultipleChoice<Bevolkingsonderzoek> onderzoeken;

	private List<Bevolkingsonderzoek> beginDataBevolkingsOnderzoeken;

	private IModel<RolDto> initieleRol;

	long totalePermissies = 0;

	private final boolean magAanpassen = ScreenitSession.get().checkPermission(Recht.GEBRUIKER_ROLLEN_BEHEREN, Actie.AANPASSEN);

	public RolEditPanel(String id, IModel<Rol> model)
	{
		super(id, model);
	}

	private void addAlgemeneRolGegevens()
	{
		Rol rol = getModelObject();

		beginDataBevolkingsOnderzoeken = new ArrayList<>(rol.getBevolkingsonderzoeken());
		ScreenitDropdown<Rol> parentRol = ComponentHelper.addDropDownChoiceINaam(rolForm, "parentRol", false, ModelUtil.listRModel(rolService.getParentRollen(rol)), false);
		parentRol.setEnabled(magAanpassen);
		parentRol.setNullValid(true);

		onderzoeken = new ScreenitListMultipleChoice<>("bevolkingsonderzoeken", new ListModel<>(getModelObject().getBevolkingsonderzoeken()),
			Arrays.asList(Bevolkingsonderzoek.values()), new EnumChoiceRenderer<>());
		onderzoeken.add(new AjaxFormComponentUpdatingBehavior("change")
		{
			@Override
			protected void onUpdate(AjaxRequestTarget target)
			{
				List<Recht> rechten = autorisatieService.getRechtWithBevolkingsonderzoek((List<Bevolkingsonderzoek>) onderzoeken.getConvertedInput());
				rechtDropdown.setChoices(rechten);
				target.add(permissiesContainer);
			}
		});
		onderzoeken.setRequired(true);
		onderzoeken.setEnabled(magAanpassen);
		rolForm.add(onderzoeken);

		FormComponent<String> textField = ComponentHelper.addTextField(rolForm, "naam", true, 255, false);

		Map<String, Object> restrictions = new HashMap<>();
		restrictions.put("actief", Boolean.TRUE);
		textField.setEnabled(magAanpassen);
		textField.add(new UniqueFieldValidator<>(Rol.class, rol.getId(), "naam", hibernateService, restrictions));
	}

	private ListView<Permissie> getPermissieListView()
	{
		List<Permissie> gesorteerdeList = new ArrayList<>(getModelObject().getPermissies());
		gesorteerdeList.sort(new PermissieComparator());
		getModelObject().setPermissies(gesorteerdeList);
		updateTotaalPermissies(gesorteerdeList);

		ListView<Permissie> permissiesListView = new ListView<>("permissies", new PropertyModel<>(getModel(), "permissies"))
		{
			@Override
			protected void populateItem(final ListItem<Permissie> item)
			{
				Permissie permissie = item.getModelObject();
				item.setVisible(Boolean.TRUE.equals(permissie.getActief()));
				String cssClass = "odd";
				if (item.getIndex() % 2 == 0)
				{
					cssClass = "even";
				}
				item.add(new AttributeAppender("class", Model.of(cssClass), " "));
				item.setDefaultModel(new CompoundPropertyModel<>(item.getModel()));

				WebMarkupContainer toegangLevelContainer = new WebMarkupContainer("toegangLevelContainer");
				toegangLevelContainer.setOutputMarkupId(true);

				ScreenitDropdown<ToegangLevel> toegangLevel = new ScreenitDropdown<>("toegangLevel", getToegangLevel(permissie),
					new NaamChoiceRenderer<>());
				toegangLevel.add(new OnChangeAjaxBehavior()
				{
					@Override
					protected void onUpdate(AjaxRequestTarget target)
					{

					}
				});
				toegangLevel.setRequired(true);
				toegangLevel.setEnabled(magAanpassen);
				toegangLevelContainer.add(toegangLevel);
				item.add(toegangLevelContainer);

				String bvos = "";
				if (permissie.getRecht() != null)
				{
					bvos = Bevolkingsonderzoek.getAfkortingen(permissie.getRecht().getBevolkingsonderzoeken());
				}
				item.add(new Label("rechtBvoVast", Model.of(bvos)));

				WebMarkupContainer actieContainer = new WebMarkupContainer("actieContainer");
				actieContainer.setOutputMarkupId(true);
				ScreenitDropdown<Actie> actie = new ScreenitDropdown<>("actie", getActies(permissie), new NaamChoiceRenderer<>());
				actie.setRequired(true);

				actie.add(new OnChangeAjaxBehavior()
				{
					@Override
					protected void onUpdate(AjaxRequestTarget target)
					{

					}
				});
				actie.setEnabled(magAanpassen);
				actieContainer.add(actie);
				item.add(actieContainer);

				List<Bevolkingsonderzoek> bevolkingsonderzoeken = (List<Bevolkingsonderzoek>) onderzoeken.getConvertedInput();
				if (bevolkingsonderzoeken == null)
				{
					bevolkingsonderzoeken = permissie.getRol().getBevolkingsonderzoeken();
				}
				List<Recht> rechten = autorisatieService.getRechtWithBevolkingsonderzoek(bevolkingsonderzoeken);
				if (item.getModel().getObject().getRecht() != null && !rechten.contains(item.getModel().getObject().getRecht()))
				{
					rechten.add(item.getModel().getObject().getRecht());
				}
				INaamComparator comparator = new INaamComparator();
				rechten.sort(comparator);
				rechten.remove(Recht.CLIENT_DASHBOARD);
				rechten.remove(Recht.CLIENT_GEGEVENS);
				rechten.remove(Recht.UITSTRIJKEND_ARTS);
				if (!Boolean.TRUE.equals(testModus))
				{
					rechten.remove(Recht.TESTEN);
				}

				rechtDropdownContainer = new WebMarkupContainer("rechtDropDownContainer");
				rechtDropdownContainer.setOutputMarkupId(true);
				item.add(rechtDropdownContainer);

				rechtDropdown = new ScreenitDropdown<>("recht", rechten, new ChoiceRenderer<>()
				{
					private static final long serialVersionUID = 1L;

					@Override
					public Object getDisplayValue(Recht object)
					{
						return getOmschrijvenEnAfkortingenBevolkingsonderzoeken(object);
					}
				});
				rechtDropdown.setRequired(true);
				rechtDropdown.setOutputMarkupId(true);
				rechtDropdown.setVisible(permissie.getId() == null);
				rechtDropdown.add(new AjaxFormComponentUpdatingBehavior("change")
				{
					@Override
					protected void onUpdate(AjaxRequestTarget target)
					{
						Permissie permissie = item.getModelObject();
						actie.setChoices(getActies(permissie));
						toegangLevel.setChoices(getToegangLevel(permissie));
						target.add(toegangLevelContainer);
						target.add(actieContainer);
					}
				});
				rechtDropdownContainer.add(rechtDropdown);

				WebMarkupContainer rechtVast = new EmptyPanel("rechtVast");
				rechtVast.setVisible(false);
				if (permissie.getId() != null)
				{
					rechtVast = new WebMarkupContainer("rechtVast");
					rechtVast.add(new Label("rechtNaam", new PropertyModel<>(item.getModel(), "recht.naam")));
				}
				rechtDropdownContainer.add(rechtVast);

				final WebMarkupContainer verwijderColumn = new WebMarkupContainer("verwijderColumn");
				verwijderColumn.setVisible(magAanpassen);
				FormulierIndicatingAjaxSubmitLink permissieVerwijderen = new FormulierIndicatingAjaxSubmitLink("permissieVerwijderen", rolForm)
				{
					@Override
					protected void onSubmit(AjaxRequestTarget target)
					{
						verwijderPermissie(item.getModelObject(), target);
					}

					@Override
					protected void onBeforeHandleEvent()
					{
						RolEditPanel.this.rolForm.setControleerVeplicht(Boolean.FALSE);
						super.onBeforeHandleEvent();
					}
				};
				permissieVerwijderen.setVisible(totalePermissies > 1);

				verwijderColumn.add(permissieVerwijderen);
				item.add(verwijderColumn);
			}
		};
		permissiesListView.setOutputMarkupId(true);
		return permissiesListView;
	}

	private static String getOmschrijvenEnAfkortingenBevolkingsonderzoeken(Recht recht)
	{
		StringBuilder builder = new StringBuilder(recht.getNaam());
		if (recht.getBevolkingsonderzoeken().length > 0)
		{
			builder.append(" - ");
			builder.append(recht.getBevolkingsonderzoeken()[0].getAfkorting());
			for (int i = 1; i < recht.getBevolkingsonderzoeken().length; i++)
			{
				builder.append(", ");
				builder.append(recht.getBevolkingsonderzoeken()[i].getAfkorting());
			}
		}
		return builder.toString();
	}

	@Override
	protected void onInitialize()
	{
		super.onInitialize();
		Rol rol = getModelObject();
		initieleRol = Model.of(rolMapper.rolToDto(rol));

		rolForm = new RolForm("rolForm", getModel());
		dialog = new BootstrapDialog("dialog");
		add(dialog);

		addAlgemeneRolGegevens();
		permissiesContainer = new WebMarkupContainer("permissiesContainer");
		permissiesContainer.setOutputMarkupId(true);
		permissiesContainer.add(getPermissieListView());

		final WebMarkupContainer verwijderHeader = new WebMarkupContainer("verwijderHeader");
		verwijderHeader.setVisible(magAanpassen);
		permissiesContainer.add(verwijderHeader);
		rolForm.add(permissiesContainer);

		FormulierIndicatingAjaxSubmitLink opslaan = opslaanAjaxLink();

		opslaan.setVisible(magAanpassen);
		rolForm.add(opslaan);

		AjaxLink<Rol> inActiveren = new ConfirmingIndicatingAjaxLink<>("inActiveren", dialog, "question.remove.rol")
		{
			@Override
			public void onClick(AjaxRequestTarget target)
			{
				rolService.setRechtActiefOfInactief(RolEditPanel.this.getModelObject(), ScreenitSession.get().getLoggedInAccount());
				setResponsePage(RollenBeheer.class);
			}

			@Override
			protected boolean skipConfirmation()
			{
				return Boolean.FALSE.equals(RolEditPanel.this.getModelObject().getActief());
			}
		};

		Label labelActiverenKnop = new Label("inActiverenTitle", "Inactiveren");
		if (Boolean.FALSE.equals(rol.getActief()))
		{
			labelActiverenKnop = new Label("inActiverenTitle", "Activeren");
		}
		inActiveren.add(labelActiverenKnop);
		inActiveren.setVisible(magAanpassen);
		add(inActiveren);
		rolForm.add(inActiveren);

		AjaxLink<Rol> annuleren = new AjaxLink<>("annuleren")
		{
			@Override
			public void onClick(AjaxRequestTarget target)
			{
				((RollenBeheer) getPage()).addOrReplaceContentWith(new RollenOverzichtPanel(RollenBeheer.CONTENT_ID));
			}
		};
		rolForm.add(annuleren);

		IndicatingAjaxLink<Rol> permissieToevoegenKnop = new IndicatingAjaxLink<>("permissieToevoegen", getModel())
		{
			@Override
			public void onClick(AjaxRequestTarget target)
			{
				Rol rol = getModelObject();

				Permissie nieuwPermissie = new Permissie(rol);
				rol.getPermissies().add(nieuwPermissie);
				updateTotaalPermissies(rol.getPermissies());
				target.add(permissiesContainer);
				target.add(rolForm);
			}
		};
		permissieToevoegenKnop.setVisible(magAanpassen);
		rolForm.add(permissieToevoegenKnop);
		add(rolForm);
	}

	private FormulierIndicatingAjaxSubmitLink opslaanAjaxLink()
	{
		return new FormulierIndicatingAjaxSubmitLink("opslaan", rolForm)
		{
			@Override
			protected void onSubmit(AjaxRequestTarget target)
			{
				Rol rol = getModelObject();
				List<String> messages = new ArrayList<>();
				if (heeftRechtVanNietToegestaandeBVOs())
				{
					error(getLocalizer().getString("error.recht.nvt.bvo", this));
					return;
				}
				Map<String, List<Bevolkingsonderzoek>> resultaten = vergelijkArrays(beginDataBevolkingsOnderzoeken, rol.getBevolkingsonderzoeken());

				if (rol.getId() != null && (!resultaten.get("toegevoegd").isEmpty() || !resultaten.get("verwijderd").isEmpty())
					&& medewerkerService.zijnErInstellingGebruikersMetRol(rol))
				{
					voegMessagesToe(rol, messages, resultaten);
				}
				confirmBox(target, messages, resultaten.get("verwijderd"));
			}

			private void voegMessagesToe(Rol rol, List<String> messages, Map<String, List<Bevolkingsonderzoek>> resultaten)
			{
				if (!resultaten.get("toegevoegd").isEmpty())
				{
					messages.add(getString("question.rol.toevoegen.bevolkingsonderzoek"));
				}
				if (!resultaten.get("verwijderd").isEmpty())
				{
					List<InstellingGebruikerRol> rollen;
					rollen = medewerkerService.getInstellingGebruikersMetRolEnBvos(rol, resultaten.get("verwijderd"));
					if (!rollen.isEmpty())
					{
						messages.add(getString("question.rol.remove.bevolkingsonderzoek"));
					}
					if (rolService.zijnErRechtenDieVerwijderdWorden(rol))
					{
						messages.add(getString("question.remove.rol.recht"));
					}
				}
			}

			@Override
			protected void onBeforeHandleEvent()
			{
				RolEditPanel.this.rolForm.setControleerVeplicht(Boolean.TRUE);
				super.onBeforeHandleEvent();
			}
		};
	}

	@Override
	protected void onDetach()
	{
		super.onDetach();
		ModelUtil.nullSafeDetach(initieleRol);
	}

	private void confirmBox(AjaxRequestTarget target, List<String> messages, final List<Bevolkingsonderzoek> verwijderdeOnderzoeken)
	{
		if (!messages.isEmpty())
		{
			dialog.setContent(new RolConfirmPanel(IDialog.CONTENT_ID, Model.of("Rol opslaan"), new ListModel<>(messages), new DefaultConfirmCallback()
			{
				@Override
				public void onYesClick(AjaxRequestTarget target)
				{
					opslaan(verwijderdeOnderzoeken);
				}
			}, dialog));
			dialog.open(target);
			return;
		}
		opslaan(verwijderdeOnderzoeken);
	}

	private void opslaan(List<Bevolkingsonderzoek> verwijderdeOnderzoeken)
	{
		var rol = getModelObject();
		List<InstellingGebruikerRol> rollen = new ArrayList<>();
		if (rol.getId() != null)
		{
			rollen = medewerkerService.getInstellingGebruikersMetRolEnBvos(rol, verwijderdeOnderzoeken);
		}

		var opslaan = rolService.opslaan(rol, rollen, initieleRol.getObject(), verwijderdeOnderzoeken, ScreenitSession.get().getLoggedInInstellingGebruiker());
		if (opslaan)
		{
			clearAuthorizationCache();
			ScreenitSession.get().info(getLocalizer().getString("action.save.rol", this));
			setResponsePage(RollenBeheer.class);
		}
		else
		{
			error(getLocalizer().getString("error.geen.rechten.door.bvo", this));
		}
	}

	private boolean heeftRechtVanNietToegestaandeBVOs()
	{
		final Collection<Bevolkingsonderzoek> input = onderzoeken.getConvertedInput();
		return getModelObject()
			.getPermissies()
			.stream()
			.filter(permissie -> permissie.getRecht() != null && Boolean.TRUE.equals(permissie.getActief()))
			.anyMatch(permissie -> Arrays.stream(permissie.getRecht().getBevolkingsonderzoeken())
				.noneMatch(bevolkingsonderzoek -> input.contains(bevolkingsonderzoek)));
	}

	private void verwijderPermissie(Permissie permissieOmTeVerwijderen, AjaxRequestTarget target)
	{
		permissieOmTeVerwijderen.setActief(false);
		Rol rol = permissieOmTeVerwijderen.getRol();

		boolean erZijnNogActievePermissiesOver = false;
		List<Permissie> permissies = rol.getPermissies();
		for (Permissie permissie : permissies)
		{
			if (Boolean.TRUE.equals(permissie.getActief()))
			{
				erZijnNogActievePermissiesOver = true;
				break;
			}
		}

		if (!erZijnNogActievePermissiesOver)
		{
			Permissie nieuwPermissie = new Permissie(rol);
			permissies.add(nieuwPermissie);
		}
		updateTotaalPermissies(permissies);
		target.add(permissiesContainer);
		target.add(rolForm);
	}

	private void updateTotaalPermissies(List<Permissie> permissies)
	{
		totalePermissies = permissies.stream().filter(p -> Boolean.TRUE.equals(p.getActief())).count();
	}

	private Map<String, List<Bevolkingsonderzoek>> vergelijkArrays(final List<Bevolkingsonderzoek> oudeOnderzoeken, final List<Bevolkingsonderzoek> nieuweOnderzoeken)
	{
		var map = new HashMap<String, List<Bevolkingsonderzoek>>();
		var toegevoegd = new ArrayList<Bevolkingsonderzoek>();
		var verwijderd = new ArrayList<Bevolkingsonderzoek>();
		var gelijk = new ArrayList<Bevolkingsonderzoek>();
		for (var onderzoek : nieuweOnderzoeken)
		{
			if (oudeOnderzoeken.contains(onderzoek))
			{
				gelijk.add(onderzoek);
				oudeOnderzoeken.remove(onderzoek);
			}
			else
			{
				toegevoegd.add(onderzoek);
			}
		}
		if (!oudeOnderzoeken.isEmpty())
		{
			verwijderd.addAll(oudeOnderzoeken);
		}
		map.put("toegevoegd", toegevoegd);
		map.put("verwijderd", verwijderd);
		map.put("gelijk", gelijk);
		return map;
	}

	private List<ToegangLevel> getToegangLevel(Permissie permissie)
	{
		List<ToegangLevel> rechtNiveaus;
		if (permissie != null && permissie.getRecht() != null && permissie.getRecht().getLevel() != null)
		{
			rechtNiveaus = Arrays.asList(permissie.getRecht().getLevel());
		}
		else
		{
			rechtNiveaus = Arrays.asList(ToegangLevel.values());
		}
		return rechtNiveaus;
	}

	private List<Actie> getActies(Permissie permissie)
	{
		List<Actie> rechtTypes;
		if (permissie != null && permissie.getRecht() != null && permissie.getRecht().getActie() != null && permissie.getRecht().getActie().length > 0)
		{
			rechtTypes = Arrays.asList(permissie.getRecht().getActie());
		}
		else
		{
			rechtTypes = Arrays.asList(Actie.values());
		}
		return rechtTypes;
	}

	private void clearAuthorizationCache()
	{
		Cache<Object, AuthorizationInfo> cache = realm.getAuthorizationCache();
		if (cache != null)
		{
			cache.clear();
		}
	}

	private static class RolForm extends Form<Rol>
	{

		private Boolean controleerVeplicht;

		public RolForm(String id, IModel<Rol> model)
		{
			super(id, model);
		}

		@Override
		protected void onValidate()
		{
			super.onValidate();
			if (Boolean.FALSE.equals(getControleerVeplicht()))
			{
				clearRequiredMessages(this);

				visitChildren(Component.class, (IVisitor<Component, Boolean>) (component, visit) -> clearRequiredMessages(component));
			}
		}

		public Boolean getControleerVeplicht()
		{
			return controleerVeplicht;
		}

		public void setControleerVeplicht(Boolean controleerVeplicht)
		{
			this.controleerVeplicht = controleerVeplicht;
		}

		private void clearRequiredMessages(Component comp)
		{
			comp.getFeedbackMessages().clear(new IFeedbackMessageFilter()
			{

				private static final long serialVersionUID = 1L;

				@Override
				public boolean accept(FeedbackMessage message)
				{
					if (message.getLevel() == FeedbackMessage.ERROR)
					{
						FormComponent<?> comp = (FormComponent<?>) message.getReporter();
						if (message.getMessage() instanceof ValidationErrorFeedback)
						{
							ValidationErrorFeedback errorFeedback = (ValidationErrorFeedback) message.getMessage();
							if (errorFeedback.getError() instanceof ValidationError)
							{
								ValidationError validationError = (ValidationError) errorFeedback.getError();
								return comp.isRequired() && validationError.getKeys().contains("Required");
							}
						}
					}
					return false;
				}
			});
		}
	}
}
