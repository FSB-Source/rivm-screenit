package nl.rivm.screenit.main.web.gebruiker.screening.colon.gebieden;

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
import java.util.List;

import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.component.ScreenitForm;
import nl.rivm.screenit.main.web.component.dropdown.ScreenitDropdown;
import nl.rivm.screenit.main.web.component.table.ScreenitDataTable;
import nl.rivm.screenit.main.web.gebruiker.base.GebruikerMenuItem;
import nl.rivm.screenit.main.web.gebruiker.base.ZoekenContextMenuItem;
import nl.rivm.screenit.main.web.security.SecurityConstraint;
import nl.rivm.screenit.model.Gemeente;
import nl.rivm.screenit.model.Instelling;
import nl.rivm.screenit.model.Instelling_;
import nl.rivm.screenit.model.OrganisatieType;
import nl.rivm.screenit.model.colon.ColonIntakelocatie;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.model.enums.ToegangLevel;
import nl.rivm.screenit.service.OrganisatieZoekService;
import nl.topicuszorg.hibernate.object.helper.HibernateHelper;
import nl.topicuszorg.wicket.hibernate.SimpleHibernateModel;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.markup.html.form.AjaxSubmitLink;
import org.apache.wicket.extensions.ajax.markup.html.IndicatingAjaxButton;
import org.apache.wicket.extensions.markup.html.repeater.data.grid.ICellPopulator;
import org.apache.wicket.extensions.markup.html.repeater.data.table.AbstractColumn;
import org.apache.wicket.extensions.markup.html.repeater.data.table.IColumn;
import org.apache.wicket.extensions.markup.html.repeater.data.table.PropertyColumn;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.form.ChoiceRenderer;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.markup.html.form.TextField;
import org.apache.wicket.markup.repeater.Item;
import org.apache.wicket.model.CompoundPropertyModel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.model.PropertyModel;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.wicketstuff.shiro.ShiroConstraint;

import static nl.rivm.screenit.model.Gemeente_.CODE;
import static nl.rivm.screenit.model.Gemeente_.NAAM;

@SecurityConstraint(
	actie = Actie.INZIEN,
	constraint = ShiroConstraint.HasPermission,
	checkScope = false,
	level = ToegangLevel.REGIO,
	recht = Recht.GEBRUIKER_BEHEER_GEBIEDEN,
	bevolkingsonderzoekScopes = { Bevolkingsonderzoek.COLON, Bevolkingsonderzoek.CERVIX })
@ZoekenContextMenuItem
public class GemeenteZoeken extends GebiedenBeheerPage
{

	private static final long serialVersionUID = 1L;

	private final ScreenitForm<Gemeente> zoekForm;

	@SpringBean
	private OrganisatieZoekService organisatieZoekService;

	private IModel<ColonIntakelocatie> selectedIntakelocatie = new SimpleHibernateModel<>();

	public GemeenteZoeken()
	{

		final WebMarkupContainer refreshContainer = new WebMarkupContainer("refreshContainer");
		refreshContainer.setOutputMarkupId(Boolean.TRUE);
		add(refreshContainer);

		IModel<Gemeente> criteriaModel;
		if (ScreenitSession.get().isZoekObjectGezetForComponent(GemeenteZoeken.class))
		{
			criteriaModel = (IModel<Gemeente>) ScreenitSession.get().getZoekObject(GemeenteZoeken.class);
		}
		else
		{
			Gemeente zoekObject = new Gemeente();
			ToegangLevel toeganglevel = ScreenitSession.get().getToegangsLevel(Actie.INZIEN, Recht.GEBRUIKER_BEHEER_GEBIEDEN);
			if (toeganglevel == ToegangLevel.REGIO)
			{
				zoekObject.setScreeningOrganisatie(ScreenitSession.get().getScreeningOrganisatie());
			}
			criteriaModel = ModelUtil.cModel(zoekObject);
		}
		List<IColumn<Gemeente, String>> columns = new ArrayList<>();
		columns.add(new PropertyColumn<Gemeente, String>(Model.of("Naam gemeente"), NAAM, NAAM));
		columns.add(new PropertyColumn<Gemeente, String>(Model.of("Code"), CODE, CODE));
		columns.add(new AbstractColumn<Gemeente, String>(Model.of("Aantal gebieden"))
		{

			private static final long serialVersionUID = 1L;

			@Override
			public void populateItem(Item<ICellPopulator<Gemeente>> cellItem, String componentId, IModel<Gemeente> rowModel)
			{
				cellItem.add(new Label(componentId, Model.of(rowModel.getObject().getUitnodigingsGebieden().size())));

			}
		});

		ScreenitDataTable<Gemeente, String> gemeentes = new ScreenitDataTable<Gemeente, String>("gemeentes", columns, new GemeenteDataProvider(criteriaModel, NAAM), 10,
			new Model<>("gemeentes"))
		{
			private static final long serialVersionUID = 1L;

			@Override
			public void onClick(AjaxRequestTarget target, IModel<Gemeente> model)
			{
				Gemeente gemeente = model.getObject();
				setResponsePage(new GemeenteGegevens(ModelUtil.cRModel(gemeente)));
			}

		};
		refreshContainer.add(gemeentes);

		setDefaultModel(new CompoundPropertyModel<>(criteriaModel));
		zoekForm = new ScreenitForm<Gemeente>("zoekForm", (IModel<Gemeente>) getDefaultModel());
		add(zoekForm);

		zoekForm.add(new TextField<>("naam"));

		AjaxSubmitLink submitLink = new AjaxSubmitLink("zoeken", zoekForm)
		{
			private static final long serialVersionUID = 1L;

			@Override
			protected void onSubmit(AjaxRequestTarget target)
			{
				super.onSubmit(target);
				ScreenitSession.get().setZoekObject(GemeenteZoeken.class, zoekForm.getModel());
				target.add(refreshContainer);
			}
		};
		zoekForm.add(submitLink);
		zoekForm.setDefaultButton(submitLink);

		intakelocatieForm();
	}

	private void intakelocatieForm()
	{
		Form<ColonIntakelocatie> form = new ScreenitForm<>("intakelocatieForm");
		add(form);
		var organisaties = organisatieZoekService.zoekOrganisaties(new Instelling(), List.of(OrganisatieType.INTAKELOCATIE), null,
			ScreenitSession.get().getLoggedInInstellingGebruiker(), -1, -1, Instelling_.NAAM, true);
		var intakelocaties = new ArrayList<ColonIntakelocatie>();
		organisaties.forEach(org -> intakelocaties.add((ColonIntakelocatie) HibernateHelper.deproxy(org)));
		IModel<List<ColonIntakelocatie>> values = ModelUtil.listRModel(intakelocaties, false);
		ScreenitDropdown<ColonIntakelocatie> intakelocatieSelect = new ScreenitDropdown<>("intakelocatie", new PropertyModel<ColonIntakelocatie>(this, "selectedIntakelocatie"),
			values);

		intakelocatieSelect.setChoiceRenderer(new ChoiceRenderer<>("naam"));
		intakelocatieSelect.setRequired(true);
		form.add(intakelocatieSelect);

		form.add(new IndicatingAjaxButton("verder")
		{
			private static final long serialVersionUID = 1L;

			@Override
			protected void onSubmit(AjaxRequestTarget target)
			{
				setResponsePage(new AdherentieIntakelocatie(ModelUtil.cModel(getSelectedIntakelocatie())));
			}
		});
	}

	public ColonIntakelocatie getSelectedIntakelocatie()
	{
		return ModelUtil.nullSafeGet(selectedIntakelocatie);
	}

	public void setSelectedIntakelocatie(ColonIntakelocatie selectedIntakelocatie)
	{
		this.selectedIntakelocatie = ModelUtil.nullSafeSet(selectedIntakelocatie);
	}

	@Override
	public void detachModels()
	{
		super.detachModels();
		ModelUtil.nullSafeDetach(selectedIntakelocatie);
	}

	@Override
	protected List<GebruikerMenuItem> getContextMenuItems()
	{
		List<GebruikerMenuItem> contextMenuItems = new ArrayList<GebruikerMenuItem>();
		contextMenuItems.add(new GebruikerMenuItem("menu.beheer.gemeente.zoeken", GemeenteZoeken.class));
		return contextMenuItems;
	}
}
