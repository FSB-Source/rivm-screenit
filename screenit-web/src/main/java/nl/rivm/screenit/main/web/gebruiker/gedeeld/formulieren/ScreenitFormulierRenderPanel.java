
package nl.rivm.screenit.main.web.gebruiker.gedeeld.formulieren;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
 * %%
 * Copyright (C) 2012 - 2022 Facilitaire Samenwerking Bevolkingsonderzoek
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

import java.io.Serializable;
import java.util.Date;
import java.util.List;

import javax.persistence.TemporalType;

import nl.dries.wicket.hibernate.dozer.DozerModel;
import nl.rivm.screenit.main.model.formulieren.BeanAntwoordVraagInstantieImpl;
import nl.rivm.screenit.main.model.formulieren.UnitBeanAntwoordVraagDefintieImpl;
import nl.rivm.screenit.main.web.component.ComponentHelper;
import nl.rivm.screenit.main.web.component.TextFieldNaastNullFlavourField;
import nl.rivm.screenit.main.web.component.dropdown.ScreenitDropdown;
import nl.rivm.screenit.main.web.component.dropdown.ScreenitListMultipleChoice;
import nl.rivm.screenit.main.web.gebruiker.clienten.verslag.DSValueConverter;
import nl.rivm.screenit.model.formulieren.PalgaLabel;
import nl.rivm.screenit.model.formulieren.PalgaNumberAntwoord;
import nl.rivm.screenit.model.formulieren.RomanHerhalingLabel;
import nl.rivm.screenit.model.formulieren.ScreenitFormulierInstantie;
import nl.rivm.screenit.model.formulieren.SimpleAntwoordKeuzeVraagDefinitieImpl;
import nl.rivm.screenit.model.formulieren.TypeFormulier;
import nl.rivm.screenit.model.verslag.DSValue;
import nl.topicuszorg.formulieren2.api.definitie.VraagDefinitie;
import nl.topicuszorg.formulieren2.api.instantie.FormulierElement;
import nl.topicuszorg.formulieren2.api.instantie.VraagInstantie;
import nl.topicuszorg.formulieren2.api.instantie.containers.HerhalendBlok;
import nl.topicuszorg.formulieren2.api.instantie.containers.KolomContainer;
import nl.topicuszorg.formulieren2.api.instantie.containers.SamengesteldeVraagBlok;
import nl.topicuszorg.formulieren2.api.resultaat.Antwoord;
import nl.topicuszorg.formulieren2.api.resultaat.EnkelvoudigAntwoord;
import nl.topicuszorg.formulieren2.api.resultaat.FormulierResultaat;
import nl.topicuszorg.formulieren2.beanantwoord.BeanAntwoordVraagInstantie;
import nl.topicuszorg.formulieren2.expressie.ExpressieSettings;
import nl.topicuszorg.formulieren2.persistence.definitie.VraagDefinitieImpl;
import nl.topicuszorg.formulieren2.persistence.instantie.containers.HerhalendBlokImpl;
import nl.topicuszorg.formulieren2.persistence.instantie.containers.SamengesteldeVraagBlokImpl;
import nl.topicuszorg.formulieren2.persistence.resultaat.FormulierResultaatImpl;
import nl.topicuszorg.formulieren2.wicket.interfaces.HibernateModelFactory;
import nl.topicuszorg.formulieren2.wicket.interfaces.IWicketModelFactory;
import nl.topicuszorg.formulieren2.wicketrenderer.FormulierRenderPanel;
import nl.topicuszorg.formulieren2.wicketrenderer.containers.KolomContainerPanel;
import nl.topicuszorg.formulieren2.wicketrenderer.containers.SamengesteldeVraagBlokPanel;
import nl.topicuszorg.formulieren2.wicketrenderer.impl.DefaultFormulierRenderFactory;
import nl.topicuszorg.formulieren2.wicketrenderer.impl.DefaultFormulierRenderFactory.ContainerPanelFactory;
import nl.topicuszorg.formulieren2.wicketrenderer.impl.FormulierRenderFactoryDecorator;
import nl.topicuszorg.formulieren2.wicketrenderer.interfaces.IFormulierRenderFactory;
import nl.topicuszorg.formulieren2.wicketrenderer.interfaces.IPanelResolver;
import nl.topicuszorg.formulieren2.wicketrenderer.interfaces.IWicketRenderContext;
import nl.topicuszorg.formulieren2.wicketrenderer.vraag.DefaultDateTimeAntwoordPanel;
import nl.topicuszorg.formulieren2.wicketrenderer.vraag.OpenVraagAntwoordPanel;
import nl.topicuszorg.formulieren2.wicketrenderer.vraag.VraagInstantiePanel;
import nl.topicuszorg.hibernate.object.model.HibernateObject;
import nl.topicuszorg.wicket.hibernate.SimpleListHibernateModel;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;
import nl.topicuszorg.wicket.model.DetachableListModel;

import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang.StringUtils;
import org.apache.wicket.behavior.AttributeAppender;
import org.apache.wicket.markup.head.CssHeaderItem;
import org.apache.wicket.markup.head.IHeaderResponse;
import org.apache.wicket.markup.head.JavaScriptHeaderItem;
import org.apache.wicket.markup.head.OnDomReadyHeaderItem;
import org.apache.wicket.markup.head.PriorityHeaderItem;
import org.apache.wicket.markup.html.form.DropDownChoice;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.markup.html.form.FormComponent;
import org.apache.wicket.markup.html.form.IChoiceRenderer;
import org.apache.wicket.markup.html.form.ListMultipleChoice;
import org.apache.wicket.markup.html.form.TextField;
import org.apache.wicket.markup.html.panel.GenericPanel;
import org.apache.wicket.markup.html.panel.Panel;
import org.apache.wicket.model.IDetachable;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.model.util.ListModel;
import org.apache.wicket.validation.IValidator;

public class ScreenitFormulierRenderPanel extends GenericPanel<ScreenitFormulierInstantie>
{

	public static class ScreenitFormulierFuncties
	{

		public Object computeBBPSSumScore(Object colonAscendens, Object colonDescendens, Object colonTransversum)
		{
			int ascendens = getValue(colonAscendens);
			int descendens = getValue(colonDescendens);
			int transversum = getValue(colonTransversum);

			if (ascendens == -1 || descendens == -1 || transversum == -1)
			{
				return new String[] { "2.16.840.1.113883.5.1008", "ASKU", "vs_BBPS_sumscore" };
			}
			else if (ascendens >= 0 || descendens >= 0 || transversum >= 0)
			{
				int sumScore = Math.max(ascendens, 0) + Math.max(descendens, 0) + Math.max(transversum, 0);
				return new String[] { "2.16.840.1.113883.2.4.3.36.77.5.103", String.valueOf(sumScore), "vs_BBPS_sumscore" };
			}

			return null;
		}

		public String dsValueCode(Object dsValue)
		{
			DSValue value = getDsValue(dsValue);
			if (value != null)
			{
				return value.getCode();
			}
			return "null";
		}

		private int getValue(Object colonBpps)
		{
			DSValue dsValue = getDsValue(colonBpps);
			if (dsValue == null)
			{
				return -2;
			}
			else if (dsValue.getCode().equals("ASKU"))
			{
				return -1;
			}
			else if (StringUtils.isNumeric(dsValue.getCode()))
			{
				return Integer.parseInt(dsValue.getCode());
			}
			else
			{
				return -2;
			}
		}

		private DSValue getDsValue(Object colonBpps)
		{
			if (colonBpps instanceof DSValue)
			{
				return (DSValue) colonBpps;
			}
			return null;
		}
	}

	private static final long serialVersionUID = 1L;

	private FormulierRenderContext formulierRenderContext;

	private FormulierRenderPanel formulierRenderPanel;

	public ScreenitFormulierRenderPanel(String id, IModel<ScreenitFormulierInstantie> formulierInstantie)
	{
		super(id, formulierInstantie);

		initExpressieSettings();

		IWicketModelFactory modelFactory = createModelFactory();
		IPanelResolver panelResolver = createPanelResolver(createRenderFactory());

		FormulierRenderContext formulierRenderContext = createFormulierRenderContext(modelFactory, panelResolver);

		final IModel<? extends FormulierResultaat> formulierRenderContextModel = createRenderContextModel(formulierInstantie.getObject(), formulierRenderContext);

		formulierRenderPanel = new FormulierRenderPanel("panel", formulierRenderContextModel, formulierRenderContext)
		{

			private static final long serialVersionUID = 1L;

			@Override
			protected boolean checkRequired()
			{
				return ScreenitFormulierRenderPanel.this.checkRequired();
			}

			@Override
			protected boolean isTitleVisible()
			{
				return ScreenitFormulierRenderPanel.this.isTitleVisible();
			}

		};
		add(formulierRenderPanel);
	}

	protected IModel<FormulierResultaatImpl> createRenderContextModel(ScreenitFormulierInstantie formulierInstantie, FormulierRenderContext formulierRenderContext)
	{
		FormulierResultaatImpl formulierResultaat = new FormulierResultaatImpl();
		formulierResultaat.setFormulierInstantie(formulierInstantie);

		final IModel<FormulierResultaatImpl> formulierRenderContextModel = formulierRenderContext.getModelFactory().mutableModel(formulierResultaat);
		return formulierRenderContextModel;
	}

	protected boolean isTitleVisible()
	{
		return true;
	}

	protected boolean checkRequired()
	{
		return true;
	}

	protected FormulierRenderContext createFormulierRenderContext(IWicketModelFactory modelFactory, IPanelResolver panelResolver)
	{
		FormulierRenderContext formulierRenderContext = new FormulierRenderContext(true);
		formulierRenderContext.setFormulierRenderFactory((IFormulierRenderFactory) panelResolver);
		formulierRenderContext.setIPanelResolver(panelResolver);
		formulierRenderContext.setModelFactory(modelFactory);

		return formulierRenderContext;
	}

	protected IPanelResolver createPanelResolver(DefaultFormulierRenderFactory renderFactory)
	{
		return new FormulierRenderFactoryDecorator(renderFactory);
	}

	protected DefaultFormulierRenderFactory createRenderFactory()
	{
		DefaultFormulierRenderFactory renderFactory = new DefaultFormulierRenderFactory()
		{

			private static final long serialVersionUID = 1L;

			@Override
			public <T> DropDownChoice<T> getDropDownChoice(String id, IModel<? extends List<? extends T>> choices, IModel<VraagInstantie<T>> vraagInstantieModel,
				IChoiceRenderer<? super T> renderer)
			{
				ScreenitDropdown dropDownChoice = ComponentHelper.newDropDownChoice(id, (IModel) choices, renderer);
				VraagInstantie<T> vraagInstantie = vraagInstantieModel.getObject();
				if (vraagInstantie.getVraagDefinitie() instanceof UnitBeanAntwoordVraagDefintieImpl)
				{
					dropDownChoice.add(new AttributeAppender("class", Model.of(" unit")));
				}
				if (StringUtils.equals("Boston Bowel Preparation Scale Sum Score", vraagInstantie.getVraagDefinitie().getVraag()))
				{
					dropDownChoice.setEnabled(false);
				}

				return dropDownChoice;
			}

			@Override
			public <T> Panel getOpenVraagAntwoordPanel(String id, final IModel<FormulierResultaat> formulierResultaat, IWicketRenderContext context,
				EnkelvoudigAntwoord<T> antwoord)
			{
				VraagDefinitie<T> vraagDefinitie = antwoord.getVraagInstantie().getVraagDefinitie();
				ScreenitFormulierInstantie formulierInstantie = (ScreenitFormulierInstantie) formulierResultaat.getObject().getFormulierInstantie();
				final TypeFormulier typeFormulier = formulierInstantie.getTypeFormulier();
				if (StringUtils.equals("T-nummer pathologie verslag", vraagDefinitie.getVraag()) || StringUtils.equals("T-nummer laboratorium", vraagDefinitie.getVraag()))
				{
					return new OpenVraagAntwoordPanel<T>(id, context.getWicketModelFactory().mutableModel(antwoord), formulierResultaat, context)
					{

						private static final long serialVersionUID = 1L;

						@Override
						protected FormComponent<T> createFormComponent(String id, Class<T> antwoordTypeClass)
						{
							final FormComponent<T> tnummerTextField = new TextField<>(id, antwoordTypeClass);

							String prefix = "T";
							if (typeFormulier == TypeFormulier.MAMMA_PA_FOLLOW_UP)
							{
								prefix = "(C|T)";
							}
							tnummerTextField.add((IValidator<? super T>) new TNummerPatternValidator(prefix));
							return tnummerTextField;
						}

					};
				}
				else if (vraagDefinitie.getAntwoordTypeClass() == Date.class && TemporalType.TIMESTAMP.name().equals(vraagDefinitie.getMetaInfoOpAntwoordTypeClass()))
				{
					return new DefaultDateTimeAntwoordPanel(id, context.getWicketModelFactory().mutableModel((Antwoord<Date>) antwoord), formulierResultaat, context);
				}
				else if (antwoord.getVraagInstantie() instanceof BeanAntwoordVraagInstantie && ((BeanAntwoordVraagInstantie) antwoord.getVraagInstantie()).getPropertyPath() != null
					&& ((BeanAntwoordVraagInstantie) antwoord.getVraagInstantie()).getPropertyPath().endsWith(".value"))
				{
					return new OpenVraagAntwoordPanel<T>(id, context.getWicketModelFactory().mutableModel(antwoord), formulierResultaat, context)
					{

						private static final long serialVersionUID = 1L;

						@Override
						protected FormComponent<T> createFormComponent(String id, Class<T> antwoordTypeClass)
						{
							final FormComponent<T> textField = new TextFieldNaastNullFlavourField<T>(id, antwoordTypeClass);
							return textField;
						}

					};
				}
				else

				{
					return super.getOpenVraagAntwoordPanel(id, formulierResultaat, context, antwoord);
				}
			}

			@Override
			public <T> ListMultipleChoice<T> getMultipleChoice(String id, IModel<? extends List<? extends T>> choices, IChoiceRenderer<? super T> renderer)
			{
				return new ScreenitListMultipleChoice<T>(id, choices, renderer);
			}

			@Override
			protected Panel getLabelPanel(String id, IModel<FormulierElement> formulierElement, IModel<FormulierResultaat> formulierResultaat)
			{
				if (formulierElement.getObject() instanceof RomanHerhalingLabel)
				{
					return new RomanHerhalingLabelPanel(id, (IModel) formulierElement);
				}
				else if (formulierElement.getObject() instanceof PalgaLabel)
				{
					return new PalgaLabelPanel(id, (IModel) formulierElement, formulierResultaat);
				}
				return super.getLabelPanel(id, formulierElement, formulierResultaat);
			}

			@Override
			public <T> Panel getFormulierElementPanel(String id, IModel<FormulierElement> formulierElement, IModel<FormulierResultaat> formulierResultaat,
				IWicketRenderContext context, Integer herhaling)
			{
				if (VraagInstantie.class.isAssignableFrom(formulierElement.getObject().getClass()))
				{
					IModel<VraagInstantie<T>> vraagInstantieModel = (IModel) formulierElement;
					return new VraagInstantiePanel<T>(id, vraagInstantieModel, formulierResultaat, context)
					{

						private static final long serialVersionUID = 1L;

						@Override
						protected boolean magAntwoordUpdaten(Object newValue)
						{
							return true;
						};
					};

				}
				else
				{
					return super.getFormulierElementPanel(id, formulierElement, formulierResultaat, context, herhaling);
				}
			}

		};

		renderFactory.getContainerPanelMapping().put(HerhalendBlok.class, new ContainerPanelFactory()
		{

			private static final long serialVersionUID = 1L;

			@SuppressWarnings({ "unchecked", "rawtypes" })
			@Override
			public <T> Panel contstructPanel(String id, IModel<T> model, IModel<FormulierResultaat> formulierResultaat, IWicketRenderContext context)
			{
				T object = model.getObject();
				int defaultHerhalingen = 1;
				if (object.getClass().isAssignableFrom(HerhalendBlokImpl.class))
				{
					HerhalendBlokImpl herhalendBlok = (HerhalendBlokImpl) object;
					VraagDefinitieImpl<?> herhalingProvider = herhalendBlok.getHerhalingProvider();
					if (herhalingProvider != null && herhalingProvider.getClass().isAssignableFrom(SimpleAntwoordKeuzeVraagDefinitieImpl.class))
					{
						SimpleAntwoordKeuzeVraagDefinitieImpl simpleVraagDef = (SimpleAntwoordKeuzeVraagDefinitieImpl) herhalingProvider;
						String identifier = simpleVraagDef.getIdentifier();
						if (identifier.equals("aantal_potjes"))
						{
							defaultHerhalingen = 0;
							for (Antwoord<?> antwoord : formulierResultaat.getObject().getAntwoorden())
							{
								if (antwoord.getClass().isAssignableFrom(PalgaNumberAntwoord.class))
								{
									defaultHerhalingen = 1;
									break;
								}
							}
						}
					}

				}
				return new ScreenitHerhalendBlokPanel(id, (IModel) model, formulierResultaat, context, defaultHerhalingen);
			}
		});
		renderFactory.getContainerPanelMapping().put(SamengesteldeVraagBlok.class, new ContainerPanelFactory()
		{

			private static final long serialVersionUID = 1L;

			@SuppressWarnings({ "unchecked", "rawtypes" })
			@Override
			public <T> Panel contstructPanel(String id, IModel<T> model, IModel<FormulierResultaat> formulierResultaat, IWicketRenderContext context)
			{
				SamengesteldeVraagBlokPanel samengesteldeVraagBlokPanel = new SamengesteldeVraagBlokPanel(id, (IModel) model, formulierResultaat, context);
				T object = model.getObject();
				if (object instanceof SamengesteldeVraagBlokImpl)
				{
					SamengesteldeVraagBlokImpl samengesteldVraagBlok = (SamengesteldeVraagBlokImpl) object;
					for (Object element : samengesteldVraagBlok.getElementen())
					{
						if (element instanceof BeanAntwoordVraagInstantieImpl)
						{
							BeanAntwoordVraagInstantieImpl vraagElement = (BeanAntwoordVraagInstantieImpl) element;
							if (vraagElement.getVraagDefinitie() != null && vraagElement.getVraagDefinitie().getPropertyPath() != null
								&& vraagElement.getVraagDefinitie().getPropertyPath().endsWith(".nullFlavour"))
							{
								samengesteldeVraagBlokPanel.add(new AttributeAppender("class", "bevatNullFlavour "));
								break;
							}
						}
					}
				}
				return samengesteldeVraagBlokPanel;
			}
		});

		renderFactory.getContainerPanelMapping().put(KolomContainer.class, new ContainerPanelFactory()
		{

			@Override
			public <T> Panel contstructPanel(String id, IModel<T> model, IModel<FormulierResultaat> formulierResultaat,
				IWicketRenderContext context)
			{
				return new KolomContainerPanel(id, (IModel) model, context, formulierResultaat)
				{
					@Override
					protected String getKolomBreedteCss()
					{
						return "span6";
					}
				};
			}
		});

		return renderFactory;
	}

	protected IWicketModelFactory createModelFactory()
	{
		IWicketModelFactory modelFactory = new HibernateModelFactory()
		{

			private static final long serialVersionUID = 1L;

			@Override
			public <T extends Serializable> IModel<T> mutableModel(T object)
			{
				if (object instanceof HibernateObject)
				{
					return new DozerModel<T>(object);
				}
				else
				{
					return new Model<T>(object);
				}
			}

			@SuppressWarnings({ "rawtypes", "unchecked" })
			@Override
			public <T extends Serializable> IModel<List<T>> listModel(List<T> objects)
			{
				if (CollectionUtils.isNotEmpty(objects))
				{
					if (objects.get(0) instanceof HibernateObject)
					{
						return new SimpleListHibernateModel(objects);
					}
					else if (objects.get(0) instanceof IDetachable)
					{
						return new DetachableListModel(objects);
					}
					else
					{
						return new ListModel(objects);
					}
				}
				return super.listModel(objects);
			}

		};
		return modelFactory;
	}

	protected void initExpressieSettings()
	{
		if (!ExpressieSettings.getNamespaces().containsKey("sff"))
		{
			ExpressieSettings.registreerFuncties("sff", new ScreenitFormulierFuncties());
		}
		ExpressieSettings.setExpressieProvider(new ScreenitExpressieProvider());
		ExpressieSettings.getResultaatConverters().put(DSValue.class, new DSValueConverter());
	}

	@Override
	public void renderHead(IHeaderResponse response)
	{
		super.renderHead(response);
		response.render(new PriorityHeaderItem(CssHeaderItem.forUrl("assets/js/libs/qtip/jquery.qtip.min.css")));
		response.render(new PriorityHeaderItem(JavaScriptHeaderItem.forUrl("assets/js/libs/qtip/jquery.qtip.min.js")));
		response.render(new OnDomReadyHeaderItem("initTooltip()"));
		response.render(new OnDomReadyHeaderItem("initNullFlavourFields()"));
	}

	@Override
	public void detachModels()
	{
		super.detachModels();

		ModelUtil.nullSafeDetach(formulierRenderContext);
		((ScreenitExpressieProvider) ExpressieSettings.getExpressieProvider()).resetCache();
	}

	public Form<Void> getForm()
	{
		return formulierRenderPanel.getForm();
	}
}
