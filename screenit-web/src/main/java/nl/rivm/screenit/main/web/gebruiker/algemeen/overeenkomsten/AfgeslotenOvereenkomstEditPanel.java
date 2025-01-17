
package nl.rivm.screenit.main.web.gebruiker.algemeen.overeenkomsten;

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

import java.util.Arrays;
import java.util.Date;
import java.util.List;

import nl.rivm.screenit.main.service.OvereenkomstService;
import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.component.ComponentHelper;
import nl.rivm.screenit.main.web.component.ScreenitIndicatingAjaxSubmitLink;
import nl.rivm.screenit.main.web.component.dropdown.ScreenitDropdown;
import nl.rivm.screenit.model.Instelling;
import nl.rivm.screenit.model.OrganisatieType;
import nl.rivm.screenit.model.ScreeningOrganisatie;
import nl.rivm.screenit.model.overeenkomsten.AbstractAfgeslotenOvereenkomst;
import nl.rivm.screenit.model.overeenkomsten.AfgeslotenMedewerkerOvereenkomst;
import nl.rivm.screenit.model.overeenkomsten.Overeenkomst;
import nl.rivm.screenit.model.overeenkomsten.OvereenkomstType;
import nl.rivm.screenit.service.InstellingService;
import nl.topicuszorg.wicket.hibernate.CglibHibernateModel;
import nl.topicuszorg.wicket.hibernate.SimpleListHibernateModel;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;
import nl.topicuszorg.wicket.input.validator.DependantDateValidator;
import nl.topicuszorg.wicket.input.validator.DependantDateValidator.Operator;

import org.apache.commons.lang.BooleanUtils;
import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.form.AjaxFormComponentUpdatingBehavior;
import org.apache.wicket.markup.head.IHeaderResponse;
import org.apache.wicket.markup.head.JavaScriptHeaderItem;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.form.CheckBox;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.markup.html.form.IChoiceRenderer;
import org.apache.wicket.markup.html.form.upload.FileUpload;
import org.apache.wicket.markup.html.form.upload.FileUploadField;
import org.apache.wicket.markup.html.panel.Panel;
import org.apache.wicket.model.CompoundPropertyModel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.util.ListModel;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.hibernate.Hibernate;
import org.wicketstuff.wiquery.ui.datepicker.DatePicker;

public abstract class AfgeslotenOvereenkomstEditPanel extends Panel
{

	private static final long serialVersionUID = 1L;

	private final IModel<AbstractAfgeslotenOvereenkomst> overeenkomstModel = new CglibHibernateModel<>();

	@SpringBean
	private OvereenkomstService overeenkomstService;

	@SpringBean
	private InstellingService instellingService;

	private final IModel<List<FileUpload>> fileUploadModel = new ListModel<>();

	@Override
	public void renderHead(IHeaderResponse response)
	{
		super.renderHead(response);
		response.render(JavaScriptHeaderItem.forUrl("assets/js/checkbox/checkboxAccorderen.js"));
	}

	public AfgeslotenOvereenkomstEditPanel(String id, AbstractAfgeslotenOvereenkomst abstractAfgeslotenOvereenkomst)
	{
		super(id);

		overeenkomstModel.setObject(abstractAfgeslotenOvereenkomst);

		add(new Label("titel", new IModel<String>()
		{

			private static final long serialVersionUID = 1L;

			@Override
			public String getObject()
			{
				if (ModelUtil.nullSafeGet(overeenkomstModel) != null && overeenkomstModel.getObject().getId() == null)
				{
					return "Nieuwe overeenkomst";
				}
				return "Bewerk overeenkomst";
			}
		}));

		AfgeslotenOvereenkomstEditForm form = new AfgeslotenOvereenkomstEditForm("form", overeenkomstModel);
		add(form);

		add(new ScreenitIndicatingAjaxSubmitLink("submit", form)
		{

			private static final long serialVersionUID = 1L;

			@Override
			protected void onSubmit(AjaxRequestTarget target)
			{
				super.onSubmit(target);

				FileUpload fileUpload = null;
				List<FileUpload> filesUploaded = fileUploadModel.getObject();
				if (filesUploaded != null && !filesUploaded.isEmpty())
				{
					fileUpload = filesUploaded.get(0);
				}

				try
				{
					overeenkomstService.saveOrUpdateOvereenkomst(overeenkomstModel.getObject(), ScreenitSession.get().fileUploadToUploadDocument(fileUpload),
						ScreenitSession.get().getLoggedInAccount());
					info(getString("message.succes"));
				}
				catch (Exception e)
				{
					error("Fout bij verwerken van geuploaded bestand.");
				}
				AfgeslotenOvereenkomstEditPanel.this.onSubmit(target);
			}
		});
	}

	public abstract void onSubmit(AjaxRequestTarget target);

	private class AfgeslotenOvereenkomstEditForm extends Form<AbstractAfgeslotenOvereenkomst>
	{

		private static final long serialVersionUID = 1L;

		public AfgeslotenOvereenkomstEditForm(String id, IModel<AbstractAfgeslotenOvereenkomst> model)
		{
			super(id, new CompoundPropertyModel<>(model));

			Instelling currentSelectedOrganisatie = ScreenitSession.get().getCurrentSelectedOrganisatie();
			OvereenkomstType[] overeenkomstTypes = getOvereenkomstTypes(model.getObject());
			List<OvereenkomstType> listOvereenkomstTypes = Arrays.asList(overeenkomstTypes);
			OrganisatieType organisatieType = currentSelectedOrganisatie != null && listOvereenkomstTypes.contains(OvereenkomstType.OVEREENKOMST)
				? currentSelectedOrganisatie.getOrganisatieType() : null;
			List<Overeenkomst> overeenkomsten = overeenkomstService.getOvereenkomsten(organisatieType, overeenkomstTypes);

			add(new ScreenitDropdown<Overeenkomst>("overeenkomst", new SimpleListHibernateModel<Overeenkomst>(overeenkomsten), new IChoiceRenderer<Overeenkomst>()
			{

				private static final long serialVersionUID = 1L;

				@Override
				public Object getDisplayValue(Overeenkomst object)
				{
					return object.getNaam();
				}

				@Override
				public String getIdValue(Overeenkomst object, int index)
				{
					return object.getId().toString();
				}

				@Override
				public Overeenkomst getObject(String id, IModel<? extends List<? extends Overeenkomst>> choices)
				{
					if (id != null)
					{
						return choices.getObject().stream().filter(o -> o.getId().toString().equals(id)).findFirst().orElse(null);
					}
					return null;
				}
			}).setRequired(true));
			DatePicker<Date> startDatum = ComponentHelper.newYearDatePicker("startDatum");
			startDatum.setRequired(true);
			add(startDatum);
			DatePicker<Date> eindDatum = ComponentHelper.newYearDatePicker("eindDatum");
			add(eindDatum);

			CheckBox teAccoderen = new CheckBox("teAccoderen")
			{

				private static final long serialVersionUID = 1L;

				@Override
				protected void onConfigure()
				{
					super.onConfigure();
					AbstractAfgeslotenOvereenkomst overeenkomst = AfgeslotenOvereenkomstEditForm.this.getModelObject();
					boolean enabled = BooleanUtils.isNotTrue(getModelObject())
						|| ((overeenkomst == null || overeenkomst.getId() == null) && listOvereenkomstTypes.contains(OvereenkomstType.KWALITEITSOVEREENKOMST));
					setEnabled(enabled);
					boolean visible = (overeenkomst != null && overeenkomst.getId() != null) || listOvereenkomstTypes.contains(OvereenkomstType.KWALITEITSOVEREENKOMST);
					setVisible(visible);
					if (visible && enabled && 
						(overeenkomst != null && overeenkomst.getGescandDocument() == null && 
							listOvereenkomstTypes.contains(OvereenkomstType.OVEREENKOMST)))
					{
						AjaxRequestTarget target = getRequestCycle().find(AjaxRequestTarget.class).orElse(null);
						if (target != null)
						{
							target.appendJavaScript("$('teAccorderen').hide();");
						}
					}
				}
			};
			add(teAccoderen);

			add(new DependantDateValidator(startDatum, eindDatum, Operator.AFTER));
			add(new ScreenitDropdown<ScreeningOrganisatie>("screeningOrganisatie",
				new SimpleListHibernateModel<>(instellingService.getActieveInstellingen(ScreeningOrganisatie.class)), new IChoiceRenderer<ScreeningOrganisatie>()
			{

				private static final long serialVersionUID = 1L;

				@Override
				public Object getDisplayValue(ScreeningOrganisatie object)
				{
					return object.getNaam();
				}

				@Override
				public String getIdValue(ScreeningOrganisatie object, int index)
				{
					return object.getId().toString();
				}

				@Override
				public ScreeningOrganisatie getObject(String id, IModel<? extends List<? extends ScreeningOrganisatie>> choices)
				{
					if (id != null)
					{
						return choices.getObject().stream().filter(o -> o.getId().toString().equals(id)).findFirst().orElse(null);
					}
					return null;
				}
			})
			{

				private static final long serialVersionUID = 1L;

				@Override
				protected void onConfigure()
				{
					super.onConfigure();
					setVisible(getModelObject() == null);
					setRequired(getModelObject() == null);
				}

			});

			add(new Label("gescandDocument.naam")
			{

				private static final long serialVersionUID = 1L;

				@Override
				protected void onConfigure()
				{
					super.onConfigure();
					AbstractAfgeslotenOvereenkomst overeenkomst = overeenkomstModel.getObject();
					setVisible(overeenkomst != null && overeenkomst.getGescandDocument() != null);
				}

			});

			var uploadField = new FileUploadField("upload", fileUploadModel)
			{

				private static final long serialVersionUID = 1L;

				@Override
				protected void onConfigure()
				{
					super.onConfigure();
					AbstractAfgeslotenOvereenkomst overeenkomst = overeenkomstModel.getObject();
					setVisible(overeenkomst != null && overeenkomst.getId() != null && 
						listOvereenkomstTypes.contains(OvereenkomstType.OVEREENKOMST));
				}

			};
			uploadField.add(new AjaxFormComponentUpdatingBehavior("change")
			{
				@Override
				protected void onUpdate(AjaxRequestTarget target)
				{
					target.appendJavaScript("showAccorderen($('.js-file-te-accorderen'))");
				}
			});
			add(uploadField);
		}

		private OvereenkomstType[] getOvereenkomstTypes(AbstractAfgeslotenOvereenkomst abstractAfgeslotenOvereenkomst)
		{
			if (Hibernate.getClass(abstractAfgeslotenOvereenkomst).equals(AfgeslotenMedewerkerOvereenkomst.class))
			{
				return new OvereenkomstType[] { OvereenkomstType.KWALITEITSOVEREENKOMST };
			}
			else
			{
				return new OvereenkomstType[] { OvereenkomstType.OVEREENKOMST, OvereenkomstType.ZAKELIJKE_OVEREENKOMST };
			}
		}
	}
}
