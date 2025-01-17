package nl.rivm.screenit.main.web.gebruiker.algemeen.organisatiemedewerker;

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
import java.util.Comparator;
import java.util.Date;
import java.util.List;
import java.util.stream.Collectors;

import nl.rivm.screenit.dto.InstellingGebruikerRolDto;
import nl.rivm.screenit.main.service.MedewerkerService;
import nl.rivm.screenit.main.service.RolService;
import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.component.ComponentHelper;
import nl.rivm.screenit.main.web.component.NaamChoiceRenderer;
import nl.rivm.screenit.main.web.component.dropdown.RequiredScreenitDropdown;
import nl.rivm.screenit.main.web.component.dropdown.ScreenitListMultipleChoice;
import nl.rivm.screenit.main.web.component.modal.IDialog;
import nl.rivm.screenit.main.web.component.table.ActiefHeaderInFormPanel;
import nl.rivm.screenit.mappers.InstellingGebruikerRolMapper;
import nl.rivm.screenit.model.InstellingGebruiker;
import nl.rivm.screenit.model.InstellingGebruikerRol;
import nl.rivm.screenit.model.Rol;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.model.helper.InstellingGebruikerRolComparator;
import nl.topicuszorg.hibernate.object.helper.HibernateHelper;
import nl.topicuszorg.wicket.hibernate.CglibListHibernateModel;
import nl.topicuszorg.wicket.hibernate.cglib.ModelProxyHelper;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;
import nl.topicuszorg.wicket.input.AttributeRemover;
import nl.topicuszorg.wicket.input.validator.DependantDateValidator;

import org.apache.commons.collections.CollectionUtils;
import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.form.AjaxFormComponentUpdatingBehavior;
import org.apache.wicket.ajax.markup.html.AjaxLink;
import org.apache.wicket.ajax.markup.html.form.AjaxSubmitLink;
import org.apache.wicket.behavior.AttributeAppender;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.form.EnumChoiceRenderer;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.markup.html.form.FormComponent;
import org.apache.wicket.markup.html.list.ListItem;
import org.apache.wicket.markup.html.list.ListView;
import org.apache.wicket.markup.html.panel.GenericPanel;
import org.apache.wicket.model.CompoundPropertyModel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.model.PropertyModel;
import org.apache.wicket.spring.injection.annot.SpringBean;

public class OrganisatieMedewerkerRollenPanel extends GenericPanel<InstellingGebruiker>
{

	private final IDialog dialogParent;

	private final WebMarkupContainer instellingGebruikersContainer;

	@SpringBean
	private RolService rolService;

	@SpringBean
	private MedewerkerService medewerkerService;

	@SpringBean
	private InstellingGebruikerRolMapper instellingGebruikerRolMapper;

	private IModel<List<InstellingGebruikerRolDto>> initieleRollen;

	private final IModel<List<InstellingGebruikerRol>> rollenModel;

	private IModel<List<Rol>> toeTeVoegenRollenModel;

	public OrganisatieMedewerkerRollenPanel(String id, IModel<InstellingGebruiker> instellingGebruikerModel, final IDialog dialogParent,
		final WebMarkupContainer instellingGebruikersContainer)
	{
		super(id, instellingGebruikerModel);

		var rollen = new ArrayList<>(instellingGebruikerModel.getObject().getRollen());
		rollen.sort(new InstellingGebruikerRolComparator());
		rollenModel = new CglibListHibernateModel<>(rollen);
		this.dialogParent = dialogParent;
		this.instellingGebruikersContainer = instellingGebruikersContainer;
	}

	@Override
	protected void onInitialize()
	{
		super.onInitialize();
		initieleRollen = Model.ofList(getModelObject().getRollen().stream().map(rol -> instellingGebruikerRolMapper.instellingGebruikerRolToDto(rol)).collect(Collectors.toList()));

		final Form<InstellingGebruiker> rollenForm = new Form<>("rollenForm");
		rollenForm.setOutputMarkupId(true);

		OrganisatieMedewerkerKoppelPage page = (OrganisatieMedewerkerKoppelPage) getPage();
		Actie actie = page.getActie(Recht.GEBRUIKER_MEDEWERKER_ORGANISATIES_BEHEER);

		final boolean inzien = actie.getNiveau() < Actie.AANPASSEN.getNiveau();

		InstellingGebruiker instellingGebruiker = getModel().getObject();
		rollenForm.add(new Label("achternaam", instellingGebruiker.getMedewerker().getAchternaamVolledig()));
		rollenForm.add(new Label("voornaam", instellingGebruiker.getMedewerker().getVoornaam()));
		rollenForm.add(new Label("organisatie", instellingGebruiker.getOrganisatie().getNaam()));

		final AjaxSubmitLink opslaan = new AjaxSubmitLink("opslaan")
		{
			@Override
			protected void onSubmit(AjaxRequestTarget target)
			{
				InstellingGebruiker instellingGebruiker = OrganisatieMedewerkerRollenPanel.this.getModelObject();
				List<Bevolkingsonderzoek> onderzoeken = new ArrayList<>();
				instellingGebruiker.setRollen(ModelProxyHelper.deproxy(rollenModel.getObject()));

				for (InstellingGebruikerRol rol : instellingGebruiker.getRollen())
				{
					if (rol.isRolActief() && CollectionUtils.isNotEmpty(rol.getBevolkingsonderzoeken()))
					{
						for (Bevolkingsonderzoek bvo : rol.getBevolkingsonderzoeken())
						{
							if (!onderzoeken.contains(bvo))
							{
								onderzoeken.add(bvo);
							}
						}
					}
				}

				if (CollectionUtils.isNotEmpty(instellingGebruiker.getBevolkingsonderzoeken()))
				{
					List<Bevolkingsonderzoek> huidigeLijst = new ArrayList<>();
					for (Bevolkingsonderzoek bvo : instellingGebruiker.getBevolkingsonderzoeken())
					{
						if (onderzoeken.contains(bvo))
						{
							huidigeLijst.add(bvo);
						}
					}
					instellingGebruiker.setBevolkingsonderzoeken(huidigeLijst);
				}

				if (CollectionUtils.isEmpty(instellingGebruiker.getBevolkingsonderzoeken()))
				{
					instellingGebruiker.setBevolkingsonderzoeken(onderzoeken);
				}

				medewerkerService.saveOrUpdateRollen(ScreenitSession.get().getLoggedInInstellingGebruiker(), initieleRollen.getObject(),
					(InstellingGebruiker) HibernateHelper.deproxy(instellingGebruiker));
				info(getLocalizer().getString("action.save.rol", this));
				OrganisatieMedewerkerKoppelPage page = (OrganisatieMedewerkerKoppelPage) getPage();
				page.clearCachedAuthorizationInfo(instellingGebruiker);
				dialogParent.close(target);
				target.add(instellingGebruikersContainer);
			}
		};
		opslaan.setVisible(!inzien);
		rollenForm.add(opslaan);

		AjaxSubmitLink toevoegen = new AjaxSubmitLink("toevoegen", rollenForm)
		{
			@Override
			protected void onSubmit(AjaxRequestTarget target)
			{
				InstellingGebruikerRol instellingGebruikerRol = new InstellingGebruikerRol();

				var rollen = new ArrayList<>(rollenModel.getObject());
				rollen.add(instellingGebruikerRol);
				rollen.sort(new InstellingGebruikerRolComparator());
				var rollenModelObject = rollenModel.getObject();
				rollenModelObject.clear();
				rollenModelObject.addAll(rollen);
				var toeTeVoegenRol = rollenModelObject.get(0);
				toeTeVoegenRol.setInstellingGebruiker(getModelObject());
				toeTeVoegenRol.setActief(true);

				target.add(rollenForm);
			}
		};
		if (actie.getNiveau() < Actie.TOEVOEGEN.getNiveau())
		{
			toevoegen.setVisible(false);
		}

		AjaxLink<Void> sluiten = new AjaxLink<>("sluiten")
		{
			@Override
			public void onClick(AjaxRequestTarget target)
			{
				dialogParent.close(target);
				target.add(instellingGebruikersContainer);
			}
		};

		rollenForm.add(sluiten);
		rollenForm.add(toevoegen);
		createRollenOverview(rollenForm, opslaan, actie);
		add(rollenForm);
	}

	private void createRollenOverview(final Form<InstellingGebruiker> rollenForm, final AjaxSubmitLink opslaan, final Actie actie)
	{
		InstellingGebruikerRol searchObject = new InstellingGebruikerRol();
		searchObject.setActief(null);
		final IModel<InstellingGebruikerRol> searchObjectModel = Model.of(searchObject);
		var toeTeVoegenRollen = rolService.getToeTeVoegenRollen(getModelObject(), ScreenitSession.get().getLoggedInInstellingGebruiker());
		toeTeVoegenRollen.sort(Comparator.comparing(Rol::getNaam));
		toeTeVoegenRollenModel = ModelUtil.listRModel(toeTeVoegenRollen, false);
		var rollenListView = new ListView<>("rollen", rollenModel)
		{
			@Override
			protected void populateItem(final ListItem<InstellingGebruikerRol> item)
			{
				final boolean inzien = actie.getNiveau() < Actie.AANPASSEN.getNiveau();
				item.setDefaultModel(new CompoundPropertyModel<>(item.getModel()));
				InstellingGebruikerRol instellingGebruikerRol = item.getModelObject();

				final List<Bevolkingsonderzoek> bvoKeuze;
				bvoKeuze = getBvoKeuzes(item);

				ScreenitListMultipleChoice<Bevolkingsonderzoek> bevolkingsonderzoeken = new ScreenitListMultipleChoice<>("bevolkingsonderzoeken",
					new PropertyModel<>(item.getDefaultModel(), "bevolkingsonderzoeken"), bvoKeuze, new EnumChoiceRenderer<>());
				bevolkingsonderzoeken.setRequired(true);

				final WebMarkupContainer bvoContainer = new WebMarkupContainer("bevolkingsonderzoekenContainer");
				bvoContainer.add(bevolkingsonderzoeken);
				bvoContainer.setOutputMarkupId(true);
				item.add(bvoContainer);

				RequiredScreenitDropdown<Rol> rol = new RequiredScreenitDropdown<>("rol",
					toeTeVoegenRollenModel, new NaamChoiceRenderer<>(), opslaan);
				rol.add(new AjaxFormComponentUpdatingBehavior("change")
				{
					@Override
					protected void onUpdate(AjaxRequestTarget target)
					{
						bvoKeuze.clear();
						bvoKeuze.addAll(getBvoKeuzes(item));
						target.add(bvoContainer);
					}
				});
				rol.setOutputMarkupId(true);
				rol.setLabel(Model.of("Rol"));
				rol.setVisible(instellingGebruikerRol.getId() == null && !inzien);
				item.add(rol);

				Label rolVast = new Label("rolVast", new PropertyModel<>(item.getModel(), "rol.naam"));
				rolVast.setVisible(instellingGebruikerRol.getId() != null || inzien);
				item.add(rolVast);

				FormComponent<Date> beginDatum = ComponentHelper.addTextField(item, "beginDatum", false, 20, Date.class, inzien);
				FormComponent<Date> eindDatum = ComponentHelper.addTextField(item, "eindDatum", false, 20, Date.class, inzien);
				beginDatum.setLabel(Model.of("begin datum"));
				eindDatum.setLabel(Model.of("eind datum"));
				rollenForm.add(new DependantDateValidator(beginDatum, eindDatum, DependantDateValidator.Operator.AFTER));

				Boolean rolActief = instellingGebruikerRol.getActief();
				WebMarkupContainer toggleActief = new AjaxLink<Void>("toggleActief")
				{
					@Override
					public void onClick(AjaxRequestTarget target)
					{
						if (actie.getNiveau() > Actie.TOEVOEGEN.getNiveau())
						{
							InstellingGebruikerRol instellingGebruikerRol = item.getModelObject();
							instellingGebruikerRol.setActief(Boolean.FALSE.equals(instellingGebruikerRol.getActief()));
							if (Boolean.TRUE.equals(instellingGebruikerRol.getActief()))
							{
								add(new AttributeRemover("class", Model.of(" niet-actief"), " "));
								add(new AttributeAppender("class", Model.of(" actief")));
							}
							else
							{
								add(new AttributeRemover("class", Model.of(" actief"), " "));
								add(new AttributeAppender("class", Model.of(" niet-actief")));
							}
							if (instellingGebruikerRol.getId() == null)
							{
								rollenModel.getObject().remove(instellingGebruikerRol);
								target.add(rollenForm);
							}
							else
							{
								target.add(this);
							}
						}
					}

				};
				toggleActief.setOutputMarkupId(true);
				if (Boolean.FALSE.equals(rolActief))
				{
					toggleActief.add(new AttributeAppender("class", Model.of(" niet-actief")));
				}
				else
				{
					toggleActief.add(new AttributeAppender("class", Model.of(" actief")));
				}
				item.add(toggleActief);
				boolean visible = false;
				Boolean searchActief = searchObjectModel.getObject().getActief();

				if (searchActief == null
					|| Boolean.TRUE.equals(searchActief) && !Boolean.FALSE.equals(rolActief)
					|| Boolean.FALSE.equals(searchActief) && Boolean.FALSE.equals(rolActief))
				{
					visible = true;
				}
				item.setVisible(visible);
			}
		};
		rollenForm.add(rollenListView);

		rollenForm.add(new ActiefHeaderInFormPanel<>("actiefHeader", rollenForm, searchObjectModel));
	}

	private List<Bevolkingsonderzoek> getBvoKeuzes(ListItem<InstellingGebruikerRol> item)
	{
		List<Bevolkingsonderzoek> bvoKeuze;
		bvoKeuze = new ArrayList<>();
		if (item.getModelObject().getRol() != null && CollectionUtils.isNotEmpty(item.getModelObject().getRol().getBevolkingsonderzoeken()))
		{
			bvoKeuze.addAll(item.getModelObject().getRol().getBevolkingsonderzoeken());
		}
		return bvoKeuze;
	}

	@Override
	protected void onDetach()
	{
		super.onDetach();
		ModelUtil.nullSafeDetach(initieleRollen);
		ModelUtil.nullSafeDetach(rollenModel);
		ModelUtil.nullSafeDetach(toeTeVoegenRollenModel);
	}
}
