package nl.rivm.screenit.main.web.gebruiker.screening.colon.overeenkomstenzoeken;

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
import java.util.List;

import nl.rivm.screenit.main.service.OvereenkomstService;
import nl.rivm.screenit.main.web.component.ComponentHelper;
import nl.rivm.screenit.main.web.component.ScreenitForm;
import nl.rivm.screenit.main.web.component.dropdown.ScreenitDropdown;
import nl.rivm.screenit.main.web.component.form.PostcodeField;
import nl.rivm.screenit.main.web.component.table.ScreenitDataTable;
import nl.rivm.screenit.main.web.gebruiker.screening.colon.ColonScreeningBasePage;
import nl.rivm.screenit.main.web.security.SecurityConstraint;
import nl.rivm.screenit.model.Instelling;
import nl.rivm.screenit.model.Instelling_;
import nl.rivm.screenit.model.OrganisatieType;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.model.enums.ToegangLevel;
import nl.rivm.screenit.model.overeenkomsten.AfgeslotenInstellingOvereenkomst;
import nl.rivm.screenit.model.overeenkomsten.Overeenkomst;
import nl.rivm.screenit.service.InstellingService;
import nl.rivm.screenit.service.OrganisatieZoekService;
import nl.topicuszorg.wicket.component.link.IndicatingAjaxSubmitLink;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang.StringUtils;
import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.extensions.markup.html.repeater.data.grid.ICellPopulator;
import org.apache.wicket.extensions.markup.html.repeater.data.table.AbstractColumn;
import org.apache.wicket.extensions.markup.html.repeater.data.table.IColumn;
import org.apache.wicket.extensions.markup.html.repeater.data.table.PropertyColumn;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.form.ChoiceRenderer;
import org.apache.wicket.markup.repeater.Item;
import org.apache.wicket.model.CompoundPropertyModel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.wicketstuff.shiro.ShiroConstraint;

@SecurityConstraint(
	actie = Actie.INZIEN,
	checkScope = false,
	constraint = ShiroConstraint.HasPermission,
	level = ToegangLevel.LANDELIJK,
	recht = Recht.GEBRUIKER_OVEREENKOMSTEN_ZOEKEN,
	bevolkingsonderzoekScopes = { Bevolkingsonderzoek.COLON })
public class OvereenkomstZoekenBeheerPage extends ColonScreeningBasePage
{
	@SpringBean
	private OvereenkomstService overeenkomstService;

	@SpringBean
	private InstellingService instellingService;

	@SpringBean
	private OrganisatieZoekService organisatieZoekService;

	private final IModel<OvereenkomstZoekFilter> filter;

	private final WebMarkupContainer resultcontainer;

	public OvereenkomstZoekenBeheerPage()
	{
		filter = new CompoundPropertyModel<>(new OvereenkomstZoekFilter());

		resultcontainer = new WebMarkupContainer("resultContainer");
		resultcontainer.setOutputMarkupId(true);

		List<IColumn<Instelling, String>> columns = new ArrayList<>();
		columns.add(new PropertyColumn<>(Model.of("Naam organisatie"), Instelling_.NAAM, Instelling_.NAAM));
		columns.add(new PropertyColumn<>(Model.of("Adres"), "adressen[0].adres"));
		columns.add(new PropertyColumn<>(Model.of("Plaats"), "adressen[0].plaats"));
		columns.add(new AbstractColumn<>(Model.of("Regio"))
		{
			@Override
			public void populateItem(Item<ICellPopulator<Instelling>> cellItem, String componentId, IModel<Instelling> rowModel)
			{
				List<Instelling> lijst = organisatieZoekService.screeningsorganisatiesWaarOrganisatieOndervalt(rowModel.getObject());
				StringBuilder sb = new StringBuilder();
				for (Instelling instelling : lijst)
				{
					sb.append(instelling.getNaam());
					sb.append(", ");
				}
				cellItem.add(new Label(componentId, StringUtils.removeEnd(sb.toString(), ", ")));
			}
		});
		columns.add(new PropertyColumn<>(Model.of("Unieke code"), Instelling_.UZI_ABONNEENUMMER)
		{
			@Override
			public void populateItem(Item<ICellPopulator<Instelling>> item, String componentId, IModel<Instelling> rowModel)
			{
				IModel<?> data = getDataModel(rowModel);
				if (data != null && data.getObject() != null)
				{
					item.add(new Label(componentId, "URA-nummer: " + data.getObject()));
				}
				else
				{
					item.add(new Label(componentId, Model.of("")));
				}
			}
		});
		columns.add(new AbstractColumn<>(Model.of("Overeenkomsten"))
		{
			@Override
			public void populateItem(Item<ICellPopulator<Instelling>> cellItem, String componentId, IModel<Instelling> rowModel)
			{
				List<AfgeslotenInstellingOvereenkomst> overeenkomsten = overeenkomstService.getAfgeslotenOvereenkomstenVanOrganisatie(filter.getObject(), rowModel.getObject());
				if (CollectionUtils.isNotEmpty(overeenkomsten))
				{
					cellItem.add(new AfgeslotenOvereenkomstenLijstPanel(componentId, ModelUtil.listRModel(overeenkomsten)));
				}
				else
				{
					cellItem.add(new Label(componentId, Model.of("")));
				}
			}
		});

		ScreenitDataTable<Instelling, String> instellingenDataTable = new ScreenitDataTable<>("instellingenDataTable", columns,
			new OvereenkomstZoekenDataProvider(filter, Instelling_.NAAM), 10, new Model<>("organisaties"));
		resultcontainer.add(instellingenDataTable);
		add(resultcontainer);

		ScreenitForm<OvereenkomstZoekFilter> overeenkomstForm = new ScreenitForm<>("zoekForm", filter);
		ComponentHelper.addTextField(overeenkomstForm, "organisatieNaam", false, 255, String.class, false);
		ComponentHelper.addTextField(overeenkomstForm, "organisatiePlaats", false, 255, String.class, false);
		ComponentHelper.addTextField(overeenkomstForm, "organisatieUra", false, 255, String.class, false);
		overeenkomstForm.add(ComponentHelper.newDatePicker("lopendeDatum"));
		overeenkomstForm.add(new PostcodeField("organisatiePostcode").setAlleenCijfersToegestaan(true));
		overeenkomstForm.add(new ScreenitDropdown<Instelling>("regio", ModelUtil.listRModel(instellingService.getAllActiefScreeningOrganisaties()), new ChoiceRenderer<Instelling>()
		{
			@Override
			public Object getDisplayValue(Instelling object)
			{
				return object.getNaam();
			}
		}).setNullValid(true));
		ComponentHelper.addDropDownChoice(overeenkomstForm, "organisatieType", false, Arrays.asList(OrganisatieType.values()), false).setNullValid(true);
		overeenkomstForm.add(new ScreenitDropdown<>("overeenkomst", ModelUtil.listRModel(overeenkomstService.getAlleOvereenkomstenVanTypeOvereenkomst()),
			new ChoiceRenderer<>()
			{
				@Override
				public Object getDisplayValue(Overeenkomst object)
				{
					if (object != null)
					{
						return object.getNaam();
					}
					else
					{
						return "";
					}
				}
			}).setNullValid(true));
		IndicatingAjaxSubmitLink zoekKnop = new IndicatingAjaxSubmitLink("zoeken", overeenkomstForm)
		{
			@Override
			protected void onSubmit(AjaxRequestTarget target)
			{
				target.add(resultcontainer);
			}
		};
		overeenkomstForm.add(zoekKnop);
		overeenkomstForm.setDefaultButton(zoekKnop);
		add(overeenkomstForm);
	}

	@Override
	protected boolean bevatFormulieren()
	{
		return Boolean.FALSE;
	}

	@Override
	protected void onDetach()
	{
		super.onDetach();
		ModelUtil.nullSafeDetach(filter);
	}

}
