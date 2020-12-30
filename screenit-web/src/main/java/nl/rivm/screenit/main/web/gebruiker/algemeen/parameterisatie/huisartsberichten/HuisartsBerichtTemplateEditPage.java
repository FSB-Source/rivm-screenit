package nl.rivm.screenit.main.web.gebruiker.algemeen.parameterisatie.huisartsberichten;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
 * %%
 * Copyright (C) 2012 - 2020 Facilitaire Samenwerking Bevolkingsonderzoek
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
import nl.rivm.screenit.service.HuisartsBerichtTemplateService;
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

	private static final long serialVersionUID = 1L;

	private IModel<HuisartsBerichtTemplateFilter> filter;

	@SpringBean
	private HuisartsBerichtTemplateService templateService;

	private WebMarkupContainer templateContainer;

	private Label selecteerEenTemplate;

	public HuisartsBerichtTemplateEditPage()
	{
		setFilter(new Model<HuisartsBerichtTemplateFilter>(new HuisartsBerichtTemplateFilter()));

		add(new FilterForm("typeForm", getFilter()));
		setTemplateContainer(new WebMarkupContainer("templateContainer"));
		getTemplateContainer().setOutputMarkupPlaceholderTag(true);
		add(getTemplateContainer());
		setSelecteerEenTemplate(new Label("selecteerEenTemplate", getString("label.selecteer.huisartsberichttype")));
		getSelecteerEenTemplate().setOutputMarkupPlaceholderTag(true);
		add(getSelecteerEenTemplate());
	}

	private class FilterForm extends Form<HuisartsBerichtTemplateFilter>
	{

		private static final long serialVersionUID = 1L;

		public FilterForm(String id, IModel<HuisartsBerichtTemplateFilter> model)
		{
			super(id, new CompoundPropertyModel<>(model));

			ScreenitDropdown<HuisartsBerichtType> typeSelectie = new ScreenitDropdown<HuisartsBerichtType>("berichtType",
				templateService.getTemplateFromBevolkingsonderzoek(ScreenitSession.get().getOnderzoeken()),
				new ChoiceRenderer<HuisartsBerichtType>()
				{

					private static final long serialVersionUID = 1L;

					@Override
					public Object getDisplayValue(HuisartsBerichtType object)
					{
						return object.getNaam();
					}
				});
			typeSelectie.add(new AjaxFormComponentUpdatingBehavior("change")
			{

				private static final long serialVersionUID = 1L;

				@Override
				protected void onUpdate(AjaxRequestTarget target)
				{
					if (getFilter().getObject() != null && getFilter().getObject().getBerichtType() != null)
					{
						HuisartsBerichtTemplate template = null;

						template = getTemplateService().getTemplateByType(getFilter().getObject().getBerichtType());
						if (template == null)
						{
							template = new HuisartsBerichtTemplate();
							template.setBerichtType(getFilter().getObject().getBerichtType());
						}

						HuisartsBerichtTemplateEditPanel editPanel = new HuisartsBerichtTemplateEditPanel("templateContainer", ModelUtil.cModel(template));
						editPanel.setOutputMarkupPlaceholderTag(true);
						getTemplateContainer().replaceWith(editPanel);
						setTemplateContainer(editPanel);
						target.add(getTemplateContainer());
						getSelecteerEenTemplate().setVisible(false);
						target.add(getSelecteerEenTemplate());
					}
					else
					{
						WebMarkupContainer editPanel = new WebMarkupContainer("templateContainer");
						editPanel.setOutputMarkupPlaceholderTag(true);
						getTemplateContainer().replaceWith(editPanel);
						setTemplateContainer(editPanel);
						getSelecteerEenTemplate().setVisible(true);
						target.add(getSelecteerEenTemplate());
						target.add(getTemplateContainer());
					}
					markeerFormulierenOpgeslagen(target);
				}
			});
			typeSelectie.setNullValid(false);
			typeSelectie.setOutputMarkupId(true);
			add(typeSelectie);
		}
	}

	public IModel<HuisartsBerichtTemplateFilter> getFilter()
	{
		return filter;
	}

	public void setFilter(IModel<HuisartsBerichtTemplateFilter> filter)
	{
		this.filter = filter;
	}

	public HuisartsBerichtTemplateService getTemplateService()
	{
		return templateService;
	}

	public void setTemplateService(HuisartsBerichtTemplateService templateService)
	{
		this.templateService = templateService;
	}

	public WebMarkupContainer getTemplateContainer()
	{
		return templateContainer;
	}

	public void setTemplateContainer(WebMarkupContainer templateContainer)
	{
		this.templateContainer = templateContainer;
	}

	public Label getSelecteerEenTemplate()
	{
		return selecteerEenTemplate;
	}

	public void setSelecteerEenTemplate(Label selecteerEenTemplate)
	{
		this.selecteerEenTemplate = selecteerEenTemplate;
	}

}
