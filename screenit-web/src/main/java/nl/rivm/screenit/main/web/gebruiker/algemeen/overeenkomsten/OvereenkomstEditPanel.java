package nl.rivm.screenit.main.web.gebruiker.algemeen.overeenkomsten;

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

import java.util.Arrays;
import java.util.List;

import nl.rivm.screenit.main.dao.OvereenkomstDao;
import nl.rivm.screenit.main.service.OvereenkomstService;
import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.component.dropdown.ScreenitDropdown;
import nl.rivm.screenit.main.web.component.validator.FileValidator;
import nl.rivm.screenit.model.OrganisatieType;
import nl.rivm.screenit.model.enums.FileType;
import nl.rivm.screenit.model.overeenkomsten.Overeenkomst;
import nl.rivm.screenit.model.overeenkomsten.OvereenkomstType;
import nl.topicuszorg.wicket.hibernate.CglibHibernateModel;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.commons.collections.CollectionUtils;
import org.apache.wicket.Component;
import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.form.AjaxFormChoiceComponentUpdatingBehavior;
import org.apache.wicket.ajax.markup.html.form.AjaxSubmitLink;
import org.apache.wicket.markup.head.IHeaderResponse;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.form.EnumChoiceRenderer;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.markup.html.form.Radio;
import org.apache.wicket.markup.html.form.RadioGroup;
import org.apache.wicket.markup.html.form.TextField;
import org.apache.wicket.markup.html.form.upload.FileUpload;
import org.apache.wicket.markup.html.form.upload.FileUploadField;
import org.apache.wicket.markup.html.panel.Panel;
import org.apache.wicket.model.CompoundPropertyModel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.model.util.ListModel;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.apache.wicket.validation.validator.StringValidator;

public abstract class OvereenkomstEditPanel extends Panel
{

	private static final long serialVersionUID = 1L;

	@SpringBean
	private OvereenkomstDao overeenkomstDao;

	private final IModel<Overeenkomst> overeenkomstModel = new CglibHibernateModel<>();

	private final IModel<List<FileUpload>> fileUploadModel = new ListModel<>();

	@SpringBean
	private OvereenkomstService overeenkomstService;

	public OvereenkomstEditPanel(String id)
	{
		super(id);
		OvereenkomstEditForm editForm = new OvereenkomstEditForm("form", overeenkomstModel);
		add(editForm);

		add(new Label("titel", new IModel<String>()
		{
			private static final long serialVersionUID = 1L;

			@Override
			public String getObject()
			{
				if (ModelUtil.nullSafeGet(overeenkomstModel) != null && overeenkomstModel.getObject().getId() == null)
				{
					return "Nieuwe modelovereenkomst";
				}
				return "Bewerk modelovereenkomst";
			}
		}));

		add(new AjaxSubmitLink("submit", editForm)
		{
			private static final long serialVersionUID = 1L;

			@Override
			protected void onSubmit(AjaxRequestTarget target)
			{
				Overeenkomst overeenkomst = overeenkomstModel.getObject();
				if (overeenkomst.getId() == null && CollectionUtils.isEmpty(fileUploadModel.getObject()))
				{
					error(getString("error.nofile"));
				}
				else if (CollectionUtils.isNotEmpty(fileUploadModel.getObject()) && fileUploadModel.getObject().size() > 1)
				{
					error(getString("error.onjuistaantalfiles"));
				}
				else if (OvereenkomstType.ZAKELIJKE_OVEREENKOMST.equals(overeenkomst.getOvereenkomst()) && overeenkomstDao.isErAlEenZakelijkOvereenkomst(overeenkomst))
				{
					error(getString("error.erisalzakelijkovereenkomst"));
				}
				else
				{
					FileUpload fileUpload = null;
					if (CollectionUtils.isNotEmpty(fileUploadModel.getObject()))
					{
						fileUpload = fileUploadModel.getObject().get(0);
					}

					overeenkomstService.saveOrUpdateOvereenkomst(overeenkomst, fileUpload, ScreenitSession.get().getLoggedInAccount());
					OvereenkomstEditPanel.this.onSubmit(target);
					info(getString("message.succes"));
				}
			}

		});
	}

	public abstract void onSubmit(AjaxRequestTarget target);

	public void updateModel(Overeenkomst overeenkomst)
	{
		overeenkomstModel.setObject(overeenkomst);
	}

	private class OvereenkomstEditForm extends Form<Overeenkomst>
	{
		private static final long serialVersionUID = 1L;

		public OvereenkomstEditForm(String id, IModel<Overeenkomst> model)
		{
			super(id, new CompoundPropertyModel<>(model));

			final WebMarkupContainer organisatieTypeContainer = new WebMarkupContainer("organisatieTypeContainer");
			organisatieTypeContainer.setOutputMarkupId(true);
			add(organisatieTypeContainer);

			RadioGroup<OvereenkomstType> radioGroup = new RadioGroup<OvereenkomstType>("overeenkomst");
			radioGroup.add(new AjaxFormChoiceComponentUpdatingBehavior()
			{

				private static final long serialVersionUID = 1L;

				@Override
				protected void onUpdate(AjaxRequestTarget target)
				{
					target.add(organisatieTypeContainer);
				}

				@Override
				public void renderHead(Component component, IHeaderResponse response)
				{
					super.renderHead(component, response);
					Overeenkomst overeenkomst = overeenkomstModel.getObject();
					if (overeenkomst != null)
					{
						OvereenkomstType overeenkomstType = overeenkomst.getOvereenkomst();
						if (overeenkomstType != null && overeenkomstType.equals(OvereenkomstType.ZAKELIJKE_OVEREENKOMST))
						{
							component.setEnabled(false);
						}
						else
						{
							component.setEnabled(true);
						}
					}
				}
			});

			radioGroup.add(new AjaxFormChoiceComponentUpdatingBehavior()
			{

				private static final long serialVersionUID = 1L;

				@Override
				protected void onUpdate(AjaxRequestTarget target)
				{
					target.add(organisatieTypeContainer);
				}
			});
			radioGroup.setRequired(true);
			add(radioGroup);
			radioGroup.add(new Radio<OvereenkomstType>("typeOvereenkomst", new Model<OvereenkomstType>(OvereenkomstType.OVEREENKOMST)));
			radioGroup.add(new Radio<OvereenkomstType>("typeKwaliteitsovereenkomst", new Model<OvereenkomstType>(OvereenkomstType.KWALITEITSOVEREENKOMST)));
			radioGroup.add(new Radio<OvereenkomstType>("typeZakelijkeovereenkomst", new Model<OvereenkomstType>(OvereenkomstType.ZAKELIJKE_OVEREENKOMST)));

			organisatieTypeContainer
				.add(new ScreenitDropdown<OrganisatieType>("organisatieType", Arrays.asList(OrganisatieType.values()), new EnumChoiceRenderer<OrganisatieType>())
				{
					private static final long serialVersionUID = 1L;

					@Override
					protected void onConfigure()
					{
						super.onConfigure();
						Overeenkomst overeenkomst = OvereenkomstEditForm.this.getModelObject();
						boolean visibleEnRequired = overeenkomst != null && overeenkomst.getOvereenkomst() == OvereenkomstType.OVEREENKOMST;
						setVisible(visibleEnRequired);
						setRequired(visibleEnRequired);
					}
				});

			add(new TextField<>("naam").setRequired(true).add(StringValidator.maximumLength(255)));
			add(new FileUploadField("upload", fileUploadModel)
				.add(new FileValidator(FileType.WORD_NIEUW)));
		}
	}
}
