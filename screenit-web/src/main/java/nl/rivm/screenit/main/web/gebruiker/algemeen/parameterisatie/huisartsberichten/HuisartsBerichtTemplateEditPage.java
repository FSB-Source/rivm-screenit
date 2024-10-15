package nl.rivm.screenit.main.web.gebruiker.algemeen.parameterisatie.huisartsberichten;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
 * %%
 * Copyright (C) 2012 - 2024 Facilitaire Samenwerking Bevolkingsonderzoek
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

import nl.rivm.screenit.main.service.HuisartsBerichtTemplateFilter;
import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.component.dropdown.ScreenitDropdown;
import nl.rivm.screenit.main.web.gebruiker.algemeen.parameterisatie.ParameterisatieBasePage;
import nl.rivm.screenit.main.web.security.SecurityConstraint;
import nl.rivm.screenit.model.HuisartsBerichtTemplate;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.HuisartsBerichtType;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.model.enums.ToegangLevel;
import nl.rivm.screenit.repository.algemeen.HuisartsBerichtTemplateRepository;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.form.AjaxFormComponentUpdatingBehavior;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.form.ChoiceRenderer;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.model.CompoundPropertyModel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.wicketstuff.shiro.ShiroConstraint;

@SecurityConstraint(
	actie = Actie.INZIEN,
	constraint = ShiroConstraint.HasPermission,
	recht = Recht.HUISARTSBERICHT_TEMPLATE,
	level = ToegangLevel.REGIO,
	bevolkingsonderzoekScopes = {
		Bevolkingsonderzoek.COLON, Bevolkingsonderzoek.CERVIX, Bevolkingsonderzoek.MAMMA })
public class HuisartsBerichtTemplateEditPage extends ParameterisatieBasePage
{

	private final IModel<HuisartsBerichtTemplateFilter> filter;

	@SpringBean
	private HuisartsBerichtTemplateRepository templateRepository;

	private WebMarkupContainer templateContainer;

	private final Label selecteerEenTemplate;

	public HuisartsBerichtTemplateEditPage()
	{
		filter = new Model<>(new HuisartsBerichtTemplateFilter());

		add(new FilterForm("typeForm", filter));
		templateContainer = new WebMarkupContainer("templateContainer");
		templateContainer.setOutputMarkupPlaceholderTag(true);
		add(templateContainer);
		selecteerEenTemplate = new Label("selecteerEenTemplate", getString("label.selecteer.huisartsberichttype"));
		selecteerEenTemplate.setOutputMarkupPlaceholderTag(true);
		add(selecteerEenTemplate);
	}

	private class FilterForm extends Form<HuisartsBerichtTemplateFilter>
	{
		public FilterForm(String id, IModel<HuisartsBerichtTemplateFilter> model)
		{
			super(id, new CompoundPropertyModel<>(model));

			ScreenitDropdown<HuisartsBerichtType> typeSelectie = new ScreenitDropdown<HuisartsBerichtType>("berichtType",
				HuisartsBerichtType.getBerichtTypeVoorBevolkingsonderzoeken(ScreenitSession.get().getOnderzoeken()),
				new ChoiceRenderer<>()
				{
					@Override
					public Object getDisplayValue(HuisartsBerichtType object)
					{
						return object.getNaam();
					}
				});
			typeSelectie.add(new AjaxFormComponentUpdatingBehavior("change")
			{
				@Override
				protected void onUpdate(AjaxRequestTarget target)
				{
					if (filter.getObject() != null && filter.getObject().getBerichtType() != null)
					{
						HuisartsBerichtTemplate template = null;

						template = templateRepository.findByBerichtType(filter.getObject().getBerichtType());
						if (template == null)
						{
							template = new HuisartsBerichtTemplate();
							template.setBerichtType(filter.getObject().getBerichtType());
						}

						HuisartsBerichtTemplateEditPanel editPanel = new HuisartsBerichtTemplateEditPanel("templateContainer", ModelUtil.cModel(template));
						editPanel.setOutputMarkupPlaceholderTag(true);
						templateContainer.replaceWith(editPanel);
						templateContainer = editPanel;
						target.add(templateContainer);
						selecteerEenTemplate.setVisible(false);
						target.add(selecteerEenTemplate);
					}
					else
					{
						WebMarkupContainer editPanel = new WebMarkupContainer("templateContainer");
						editPanel.setOutputMarkupPlaceholderTag(true);
						templateContainer.replaceWith(editPanel);
						templateContainer = editPanel;
						templateContainer.setVisible(true);
						target.add(selecteerEenTemplate);
						target.add(templateContainer);
					}
					markeerFormulierenOpgeslagen(target);
				}
			});
			typeSelectie.setNullValid(false);
			typeSelectie.setOutputMarkupId(true);
			add(typeSelectie);
		}
	}
}
