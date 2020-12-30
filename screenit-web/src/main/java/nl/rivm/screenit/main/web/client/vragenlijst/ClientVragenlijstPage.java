
package nl.rivm.screenit.main.web.client.vragenlijst;

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

import nl.rivm.screenit.main.web.base.BasePage;
import nl.rivm.screenit.main.web.base.ScreenitContext;
import nl.rivm.screenit.main.web.gebruiker.gedeeld.formulieren.FormulierIndicatingAjaxSubmitLink;
import nl.rivm.screenit.main.web.gebruiker.gedeeld.formulieren.FormulierRenderContext;
import nl.rivm.screenit.main.web.gebruiker.gedeeld.formulieren.ScreenitFormulierRenderPanel;
import nl.rivm.screenit.model.formulieren.ScreenitFormulierInstantie;
import nl.rivm.screenit.model.project.ProjectBrief;
import nl.rivm.screenit.model.project.ProjectVragenlijst;
import nl.rivm.screenit.model.project.ProjectVragenlijstAntwoordenHolder;
import nl.rivm.screenit.model.project.ProjectVragenlijstStatus;
import nl.rivm.screenit.model.vragenlijsten.VragenlijstAntwoorden;
import nl.rivm.screenit.service.ProjectService;
import nl.rivm.screenit.service.VragenlijstBaseService;
import nl.topicuszorg.formulieren2.persistence.resultaat.FormulierResultaatImpl;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.wicket.bootstrap.BootstrapFeedbackPanel;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.markup.html.form.AjaxSubmitLink;
import org.apache.wicket.behavior.AttributeAppender;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.markup.html.panel.Panel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.model.PropertyModel;
import org.apache.wicket.request.mapper.parameter.PageParameters;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.apache.wicket.util.string.StringValue;

public class ClientVragenlijstPage extends BasePage
{

	private static final long serialVersionUID = 1L;

	private final Panel feedbackPanel;

	@SpringBean
	private VragenlijstBaseService vragenlijstBaseService;

	@SpringBean
	private HibernateService hibernateService;

	@SpringBean
	private ProjectService projectService;

	private boolean checkRequired;

	private PropertyModel<FormulierResultaatImpl> formulierResultaatModel;

	private StringValue projectBriefKey;

	private StringValue isAfgerond;

	public ClientVragenlijstPage(PageParameters pageParameters)
	{
		projectBriefKey = pageParameters.get(0);
		isAfgerond = pageParameters.get(1);

		feedbackPanel = new BootstrapFeedbackPanel("feedback");
		add(feedbackPanel);

	}

	@Override
	protected void onInitialize()
	{
		super.onInitialize();

		Form<?> form = null;
		String messageKey = "geen.vragenlijst";
		if (projectBriefKey != null)
		{
			if (isAfgerond != null && "afgerond".equals(isAfgerond.toString()))
			{
				messageKey = "afronden.sucess";
			}
			else
			{
				ProjectBrief brief = projectService.getProjectBriefFromVragenlijstKey(projectBriefKey.toString());
				if (brief != null)
				{
					ProjectVragenlijst vragenlijst = brief.getDefinitie().getVragenlijst();
					IModel<ProjectBrief> dModel = ModelUtil.dModel(brief);
					setDefaultModel(dModel);
					brief = dModel.getObject();
					if (brief.getVragenlijstAntwoordenHolder() != null && brief.getVragenlijstAntwoordenHolder().getStatus().equals(ProjectVragenlijstStatus.AFGEROND))
					{
						messageKey = "vragenlijst.al.ingevuld";
					}
					else
					{
						ScreenitFormulierRenderPanel formulierRenderPanel = addFormulier(brief, vragenlijst);
						form = formulierRenderPanel.getForm();
					}
				}
			}
		}
		if (form == null)
		{
			add(new Label("vragenlijst", getString(messageKey)).add(new AttributeAppender("style", Model.of("padding: 15px;"))));
			add(new Label("naam", ""));
		}
		add(new WebMarkupContainer("inleiding").setVisible(form != null));

		addAfrondenButton(form);
		addOpslaanButton(form);
	}

	private ScreenitFormulierRenderPanel addFormulier(ProjectBrief brief, ProjectVragenlijst vragenlijst)
	{
		ScreenitFormulierInstantie formulierInstantie = null;
		ProjectVragenlijstAntwoordenHolder vragenlijstAntwoordenHolder = brief.getVragenlijstAntwoordenHolder();
		VragenlijstAntwoorden<ProjectVragenlijstAntwoordenHolder> vragenlijstAntwoorden = null;
		if (vragenlijstAntwoordenHolder == null)
		{
			vragenlijstAntwoordenHolder = new ProjectVragenlijstAntwoordenHolder();
			vragenlijstAntwoordenHolder.setStatus(ProjectVragenlijstStatus.IN_BEWERKING);
			brief.setVragenlijstAntwoordenHolder(vragenlijstAntwoordenHolder);

			vragenlijstAntwoordenHolder.setVragenlijst(vragenlijst);
			vragenlijstAntwoorden = new VragenlijstAntwoorden<ProjectVragenlijstAntwoordenHolder>();
			vragenlijstAntwoordenHolder.setVragenlijstAntwoorden(vragenlijstAntwoorden);

			vragenlijstAntwoorden.setAntwoordenHolder(vragenlijstAntwoordenHolder);
			formulierInstantie = vragenlijst.getFormulierInstantie();
			vragenlijstAntwoorden.setFormulierInstantie(formulierInstantie);

			hibernateService.saveOrUpdateAll(vragenlijstAntwoorden, vragenlijstAntwoordenHolder, brief);
		}
		else
		{
			vragenlijstAntwoorden = vragenlijstAntwoordenHolder.getVragenlijstAntwoorden();
			formulierInstantie = vragenlijstAntwoorden.getFormulierInstantie();

		}
		if (vragenlijstAntwoorden != null && vragenlijstAntwoorden.getResultaat() == null)
		{
			FormulierResultaatImpl resultaat = new FormulierResultaatImpl();
			resultaat.setFormulierInstantie(formulierInstantie);
			vragenlijstAntwoorden.setResultaat(resultaat);
			hibernateService.saveOrUpdateAll(resultaat, vragenlijstAntwoorden);
		}
		ScreenitFormulierRenderPanel formulierRenderPanel = new ScreenitFormulierRenderPanel("vragenlijst", ModelUtil.sModel(formulierInstantie))
		{

			private static final long serialVersionUID = 1L;

			@Override
			protected boolean isTitleVisible()
			{
				return false;
			}

			@Override
			protected boolean checkRequired()
			{
				return checkRequired;
			}

			@Override
			protected IModel<FormulierResultaatImpl> createRenderContextModel(ScreenitFormulierInstantie formulierInstantie, FormulierRenderContext formulierRenderContext)
			{
				formulierResultaatModel = new PropertyModel<FormulierResultaatImpl>(ClientVragenlijstPage.this.getDefaultModel(),
					"vragenlijstAntwoordenHolder.vragenlijstAntwoorden.resultaat");
				return formulierResultaatModel;
			}

		};

		add(formulierRenderPanel);
		add(new Label("naam", vragenlijst.getNaam()));
		return formulierRenderPanel;
	}

	private void addAfrondenButton(Form<?> form)
	{
		AjaxSubmitLink afrondenButton = new FormulierIndicatingAjaxSubmitLink("afronden", form)
		{

			private static final long serialVersionUID = 1L;

			@Override
			protected void onSubmit(AjaxRequestTarget target)
			{
				saveOrAfronden(target, true);
			}

			@Override
			protected void onBeforeHandleEvent()
			{
				checkRequired = true;
			}

		};
		afrondenButton.setVisible(form != null);
		add(afrondenButton);
	}

	protected void saveOrAfronden(AjaxRequestTarget target, boolean afronden)
	{
		ProjectBrief projectBrief = (ProjectBrief) getDefaultModelObject();
		ProjectVragenlijstAntwoordenHolder vragenlijstAntwoordenHolder = projectBrief.getVragenlijstAntwoordenHolder();
		vragenlijstAntwoordenHolder.getVragenlijstAntwoorden();
		VragenlijstAntwoorden resultaat = vragenlijstAntwoordenHolder.getVragenlijstAntwoorden();
		FormulierResultaatImpl formulierResultaat = formulierResultaatModel.getObject();
		resultaat.setResultaat(formulierResultaat);

		vragenlijstBaseService.saveOrAfronden(vragenlijstAntwoordenHolder, afronden, projectBrief.getProjectClient().getClient());
		if (afronden)
		{

			PageParameters parameters = new PageParameters();
			parameters.set(0, projectBriefKey);
			parameters.set(1, "afgerond");
			setResponsePage(ClientVragenlijstPage.class, parameters);
		}
		else
		{
			info(getString("opslaan.sucess"));
		}
	}

	private void addOpslaanButton(Form<?> form)
	{
		AjaxSubmitLink opslaanButton = new FormulierIndicatingAjaxSubmitLink("opslaan", form)
		{

			private static final long serialVersionUID = 1L;

			@Override
			protected void onSubmit(AjaxRequestTarget target)
			{
				saveOrAfronden(target, false);
			}

			@Override
			protected void onBeforeHandleEvent()
			{
				checkRequired = false;
			}

		};
		opslaanButton.setVisible(form != null);
		add(opslaanButton);
	}

	@Override
	public void refreshFeedback(AjaxRequestTarget target)
	{
		target.add(feedbackPanel);
	}

	@Override
	public ScreenitContext getContext()
	{
		return ScreenitContext.CLIENT;
	}
}
